#include <cstdint>
#include <cstring>
#include <initializer_list>

#include "fpga/sw/dma.h"
#include "sw/utils/utils.h"

constexpr uint32_t kDmaBusy      = 1 << 0;
constexpr uint32_t kDmaError     = 1 << 2;
constexpr uint32_t kGuard        = 0xa5a5a5a5;
constexpr unsigned kBulkRepeats  = 32;
constexpr unsigned kDmaWidthLog2 = 4;  // 16-byte transfers
constexpr unsigned kGuardWords   = 32;
constexpr unsigned kMaxBytes     = 16 * 1024;
constexpr unsigned kNumCases     = 4 * (sizeof(kSizes) / sizeof(kSizes[0]) + 1);
constexpr unsigned kSizes[]      = {16, 256, 4096, kMaxBytes};
constexpr unsigned kTrials       = 3;

enum class CopyMethod : uint32_t { Lsu, Dma };
enum class Direction : uint32_t { ExternalToDtcm, DtcmToExternal };

struct Result {
  CopyMethod method;
  Direction direction;
  uint32_t bytes, repetitions;
  uint64_t cycles[kTrials], instructions[kTrials];
};
static_assert(sizeof(Result) == 64);

extern "C" {
uint32_t status, result_count, failure_address, expected_value, actual_value;
int32_t dma_error;
Result results[kNumCases];
}

alignas(128) static uint32_t dtcm_buffer[kMaxBytes / 4 + 2 * kGuardWords];
// Test initializes external storage itself, no host payload is needed
alignas(128) static uint32_t external_buffer[kMaxBytes / 4 + 2 * kGuardWords]
    __attribute__((section(".ddr_bss")));
static dma_descriptor descriptor;

static void fence() { asm volatile("fence iorw, iorw" ::: "memory"); }

static uint32_t pattern(unsigned index, uint32_t seed) { return seed + index; }

static void initialize(volatile uint32_t *buffer, unsigned words, uint32_t seed, bool source) {
  for (unsigned i = 0; i < words + 2 * kGuardWords; ++i) {
    const bool payload = i >= kGuardWords && i < kGuardWords + words;
    buffer[i]          = source && payload ? pattern(i - kGuardWords, seed) : kGuard;
  }
}

static bool verify(const volatile uint32_t *buffer, unsigned words, uint32_t seed) {
  for (unsigned i = 0; i < words + 2 * kGuardWords; ++i) {
    const bool payload  = i >= kGuardWords && i < kGuardWords + words;
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

static bool measure(CopyMethod method, Direction direction, unsigned bytes, unsigned repetitions) {
  auto &result       = results[result_count];
  result.method      = method;
  result.direction   = direction;
  result.bytes       = bytes;
  result.repetitions = repetitions;
  auto *source       = direction == Direction::ExternalToDtcm ? external_buffer : dtcm_buffer;
  auto *target       = direction == Direction::ExternalToDtcm ? dtcm_buffer : external_buffer;
  for (unsigned trial = 0; trial < kTrials; ++trial) {
    const auto seed = 0xdead0000u + result_count * kTrials + trial;
    initialize(source, bytes / 4, seed, true);
    initialize(target, bytes / 4, seed, false);
    fence();
    const auto start_instructions = minstret_read();
    const auto start_cycles       = mcycle_read();
    asm volatile("" ::: "memory");
    for (unsigned repeat = 0; repeat < repetitions; ++repeat) {
      switch (method) {
        case CopyMethod::Lsu:
          std::memcpy(target + kGuardWords, source + kGuardWords, bytes);
          break;
        case CopyMethod::Dma:
          descriptor           = {};
          descriptor.src_addr  = reinterpret_cast<uintptr_t>(source + kGuardWords);
          descriptor.dst_addr  = reinterpret_cast<uintptr_t>(target + kGuardWords);
          descriptor.len_flags = dma_make_len_flags(bytes, kDmaWidthLog2, 0, 0, 0);
          dma_start(reinterpret_cast<uintptr_t>(&descriptor));
          dma_error = dma_wait_done();
          if (dma_error != 0)
            return false;
          break;
      }
      // Keep repeated LSU copies observable to the compiler
      asm volatile("" ::: "memory");
    }
    fence();
    const auto end_cycles       = mcycle_read();
    const auto end_instructions = minstret_read();
    result.cycles[trial]        = end_cycles - start_cycles;
    result.instructions[trial]  = end_instructions - start_instructions;
    if (!verify(target, bytes / 4, seed) || !verify(source, bytes / 4, seed))
      return false;
  }
  ++result_count;
  return true;
}

int main() {
  status = 1;
  if (dma_get_status() & kDmaBusy)
    return 1;
  for (auto direction : {Direction::ExternalToDtcm, Direction::DtcmToExternal}) {
    for (auto method : {CopyMethod::Lsu, CopyMethod::Dma}) {
      for (auto bytes : kSizes) {
        if (!measure(method, direction, bytes, 1))
          return 2;
      }
      if (!measure(method, direction, kMaxBytes, kBulkRepeats))
        return 3;
    }
  }
  if (result_count != kNumCases || (dma_get_status() & (kDmaBusy | kDmaError)))
    return 4;
  status = 3;
  return 0;
}
