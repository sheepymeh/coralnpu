#include <cstdint>
#include <cstring>
#include <initializer_list>

#include "fpga/sw/dma.h"
#include "sw/utils/utils.h"

constexpr unsigned kBytes          = 64 * 1024;
constexpr unsigned kGuardWords     = 32;
constexpr unsigned kTrials         = 3;
constexpr unsigned kMethods        = 5;
constexpr uint32_t kGuard          = 0xa5a5a5a5;
constexpr uint64_t kDeferredCycles = 450000;

enum class CopyMethod : uint32_t { Lsu, DmaContinuous, DmaGap64, DmaGap1024, DmaDeferred };
enum class Direction : uint32_t { ExternalToDtcm, DtcmToExternal };

struct Result {
  CopyMethod method;
  Direction direction;
  uint32_t bytes, nop_gap;
  uint64_t cycles[kTrials], instructions[kTrials];
  uint32_t polls[kTrials], reserved;
};
static_assert(sizeof(Result) == 80);

extern "C" {
uint32_t status, result_count, failure_address, expected_value, actual_value;
int32_t dma_error;
Result results[2 * kMethods];
}

alignas(128) static uint32_t dtcm_buffer[kBytes / 4 + 2 * kGuardWords];
alignas(128) static uint32_t external_buffer[kBytes / 4 + 2 * kGuardWords]
    __attribute__((section(".ddr_bss")));
static dma_descriptor descriptor;

static void fence() { asm volatile("fence iorw, iorw" ::: "memory"); }

static uint32_t pattern(unsigned index, uint32_t seed) { return (index * 0x9e3779b9u) ^ seed; }

static void initialize(volatile uint32_t *buffer, uint32_t seed, bool source) {
  for (unsigned i = 0; i < kBytes / 4 + 2 * kGuardWords; ++i) {
    const bool payload = i >= kGuardWords && i < kGuardWords + kBytes / 4;
    buffer[i]          = source && payload ? pattern(i - kGuardWords, seed) : kGuard;
  }
}

static bool verify(const volatile uint32_t *buffer, uint32_t seed) {
  for (unsigned i = 0; i < kBytes / 4 + 2 * kGuardWords; ++i) {
    const bool payload  = i >= kGuardWords && i < kGuardWords + kBytes / 4;
    const auto expected = payload ? pattern(i - kGuardWords, seed) : kGuard;
    const auto actual   = buffer[i];
    if (actual != expected) {
      failure_address = reinterpret_cast<uintptr_t>(buffer + i);
      expected_value  = expected;
      actual_value    = actual;
      return false;
    }
  }
  return true;
}

static void pollGap(CopyMethod method) {
  switch (method) {
    case CopyMethod::DmaGap64:
      asm volatile(".rept 64\n nop\n .endr" ::: "memory");
      break;
    case CopyMethod::DmaGap1024:
      asm volatile(".rept 1024\n nop\n .endr" ::: "memory");
      break;
    default:
      break;
  }
}

static bool measure(CopyMethod method, Direction direction) {
  auto &result     = results[result_count];
  result.method    = method;
  result.direction = direction;
  result.bytes     = kBytes;
  result.nop_gap   = method == CopyMethod::DmaGap64     ? 64
                     : method == CopyMethod::DmaGap1024 ? 1024
                                                        : 0;
  auto *source     = direction == Direction::ExternalToDtcm ? external_buffer : dtcm_buffer;
  auto *target     = direction == Direction::ExternalToDtcm ? dtcm_buffer : external_buffer;
  for (unsigned trial = 0; trial < kTrials; ++trial) {
    const auto seed = 0x12345678u ^ (result_count << 8) ^ trial;
    initialize(source, seed, true);
    initialize(target, seed, false);
    fence();
    const auto start_instructions = minstret_read();
    const auto start_cycles       = mcycle_read();
    uint32_t polls                = 0;
    switch (method) {
      case CopyMethod::Lsu:
        std::memcpy(target + kGuardWords, source + kGuardWords, kBytes);
        break;
      default: {
        descriptor           = {};
        descriptor.src_addr  = reinterpret_cast<uintptr_t>(source + kGuardWords);
        descriptor.dst_addr  = reinterpret_cast<uintptr_t>(target + kGuardWords);
        descriptor.len_flags = dma_make_len_flags(kBytes, 4, 0, 0, 0);
        fence();
        dma_start(reinterpret_cast<uintptr_t>(&descriptor));
        if (method == CopyMethod::DmaDeferred) {
          // Defer loop avoids LSU traffic, elapsed time bounds completion
          const auto start = mcycle_read();
          while (mcycle_read() - start < kDeferredCycles) {
          }
        }
        uint32_t dma_status = 0;
        do {
          dma_status = dma_get_status();
          ++polls;
          if (dma_status & 2)
            break;
          if (method == CopyMethod::DmaDeferred || polls == 1000000) {
            dma_error = -2;
            return false;
          }
          pollGap(method);
        } while (true);
        if (dma_status & 4) {
          dma_error = dma_status;
          return false;
        }
        break;
      }
    }
    fence();
    const auto end_cycles       = mcycle_read();
    const auto end_instructions = minstret_read();
    result.cycles[trial]        = end_cycles - start_cycles;
    result.instructions[trial]  = end_instructions - start_instructions;
    result.polls[trial]         = polls;
    if (!verify(target, seed) || !verify(source, seed))
      return false;
  }
  ++result_count;
  return true;
}

int main() {
  status = 1;
  if (dma_get_status() & 1)
    return 1;
  for (auto direction : {Direction::ExternalToDtcm, Direction::DtcmToExternal}) {
    for (auto method : {CopyMethod::Lsu, CopyMethod::DmaContinuous, CopyMethod::DmaGap64,
                        CopyMethod::DmaGap1024, CopyMethod::DmaDeferred}) {
      if (!measure(method, direction))
        return 2;
    }
  }
  if (result_count != 2 * kMethods || (dma_get_status() & 5))
    return 3;
  status = 3;
  return 0;
}
