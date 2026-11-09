#include <stdint.h>

enum BenchId {
  kIndependentLoads,
  kPointerChase,
  kIndependentStores,
  kVectorLoads,
  kVectorStores,
  kNoMemory,
};

// Host controls must survive startup clearing .bss
volatile uint32_t g_selected_bench          = UINT32_MAX;
volatile uint32_t g_buffer                  = UINT32_MAX;
volatile uint32_t g_memory_counters_enabled = 0;
volatile uint64_t g_results[6];
alignas(128) uint32_t g_scratch[32];

template <unsigned Address>
static void write_csr(uint32_t value) {
  asm volatile("csrw %0, %1" : : "i"(Address), "r"(value) : "memory");
}

template <unsigned Address>
static uint32_t read_csr() {
  uint32_t value;
  asm volatile("csrr %0, %1" : "=r"(value) : "i"(Address) : "memory");
  return value;
}

template <unsigned Address>
static uint64_t read_counter() {
  // All counters are frozen, RV32 halves are stable
  return uint64_t(read_csr<Address + 0x80>()) << 32 | read_csr<Address>();
}

// Include drain time, exclude setup and result stores from memory events
#define WINDOW_BEGIN           \
  "li t6, 0x7d\n"              \
  "fence rw, rw\n"             \
  "csrw mcountinhibit, zero\n" \
  "csrr zero, mcountinhibit\n"
#define WINDOW_END           \
  "fence rw, rw\n"           \
  "csrw mcountinhibit, t6\n" \
  "csrr zero, mcountinhibit\n"

static void independent_loads(uint32_t *buffer) {
  asm volatile("mv t3, %0\n" WINDOW_BEGIN
               ".rept 4\n"
               "lw t0, 0(t3)\n"
               "lw t1, 4(t3)\n"
               "lw t2, 8(t3)\n"
               "lw t4, 12(t3)\n"
               "lw t5, 16(t3)\n"
               "lw a0, 20(t3)\n"
               "lw a1, 24(t3)\n"
               "lw a2, 28(t3)\n"
               ".endr\n" WINDOW_END
               :
               : "r"(buffer)
               : "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "memory");
}

static void pointer_chase(uint32_t *buffer) {
  asm volatile("mv t3, %0\n" WINDOW_BEGIN
               ".rept 32\n"
               "lw t3, 0(t3)\n"
               ".endr\n" WINDOW_END
               :
               : "r"(buffer)
               : "t3", "t6", "memory");
}

static void independent_stores(uint32_t *buffer) {
  asm volatile(
      "mv t3, %0\n"
      "li t0, 42\n" WINDOW_BEGIN
      ".rept 4\n"
      "sw t0, 0(t3)\n"
      "sw t0, 4(t3)\n"
      "sw t0, 8(t3)\n"
      "sw t0, 12(t3)\n"
      "sw t0, 16(t3)\n"
      "sw t0, 20(t3)\n"
      "sw t0, 24(t3)\n"
      "sw t0, 28(t3)\n"
      ".endr\n" WINDOW_END
      :
      : "r"(buffer)
      : "t0", "t3", "t6", "memory");
}

#define VECTOR_SETUP                    \
  "vsetivli zero, 4, e32, m1, ta, ma\n" \
  "mv a0, %0\n"                         \
  "addi a1, a0, 16\n"                   \
  "addi a2, a0, 32\n"                   \
  "addi a3, a0, 48\n"                   \
  "addi a4, a0, 64\n"                   \
  "addi a5, a0, 80\n"                   \
  "addi a6, a0, 96\n"                   \
  "addi a7, a0, 112\n"

static void vector_loads(uint32_t *buffer) {
  asm volatile(VECTOR_SETUP WINDOW_BEGIN
               ".rept 4\n"
               "vle32.v v8, (a0)\n"
               "vle32.v v9, (a1)\n"
               "vle32.v v10, (a2)\n"
               "vle32.v v11, (a3)\n"
               "vle32.v v12, (a4)\n"
               "vle32.v v13, (a5)\n"
               "vle32.v v14, (a6)\n"
               "vle32.v v15, (a7)\n"
               ".endr\n" WINDOW_END
               :
               : "r"(buffer)
               : "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t6", "v8", "v9", "v10", "v11",
                 "v12", "v13", "v14", "v15", "memory");
}

static void vector_stores(uint32_t *buffer) {
  asm volatile(VECTOR_SETUP "vmv.v.i v8, 7\n" WINDOW_BEGIN
                            ".rept 4\n"
                            "vse32.v v8, (a0)\n"
                            "vse32.v v8, (a1)\n"
                            "vse32.v v8, (a2)\n"
                            "vse32.v v8, (a3)\n"
                            "vse32.v v8, (a4)\n"
                            "vse32.v v8, (a5)\n"
                            "vse32.v v8, (a6)\n"
                            "vse32.v v8, (a7)\n"
                            ".endr\n" WINDOW_END
               :
               : "r"(buffer)
               : "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t6", "v8", "memory");
}

static void no_memory() {
  asm volatile(WINDOW_BEGIN
               ".rept 32\n"
               "nop\n"
               ".endr\n" WINDOW_END
               :
               :
               : "t6", "memory");
}

int main() {
  write_csr<0x320>(0x7d);
  for (unsigned i = 0; i < 32; ++i) {
    g_scratch[i] = reinterpret_cast<uintptr_t>(&g_scratch[(i + 1) % 32]);
  }
  uint32_t *buffer = g_buffer == UINT32_MAX ? g_scratch : reinterpret_cast<uint32_t *>(g_buffer);

  write_csr<0x323>(2);   // LSU dispatch stalls
  write_csr<0x324>(8);   // LSU bus transfers
  write_csr<0x325>(9);   // LSU bus wait cycles
  write_csr<0x326>(10);  // LSU backlog with idle bus
  g_memory_counters_enabled = read_csr<0x324>() == 8;

  write_csr<0xb00>(0);
  write_csr<0xb80>(0);
  write_csr<0xb02>(0);
  write_csr<0xb82>(0);
  write_csr<0xb03>(0);
  write_csr<0xb83>(0);
  write_csr<0xb04>(0);
  write_csr<0xb84>(0);
  write_csr<0xb05>(0);
  write_csr<0xb85>(0);
  write_csr<0xb06>(0);
  write_csr<0xb86>(0);

  switch (g_selected_bench) {
    case kIndependentLoads:
      independent_loads(buffer);
      break;
    case kPointerChase:
      pointer_chase(buffer);
      break;
    case kIndependentStores:
      independent_stores(buffer);
      break;
    case kVectorLoads:
      vector_loads(buffer);
      break;
    case kVectorStores:
      vector_stores(buffer);
      break;
    case kNoMemory:
      no_memory();
      break;
    default:
      return 1;
  }

  g_results[0] = read_counter<0xb00>();
  g_results[1] = read_counter<0xb02>();
  g_results[2] = read_counter<0xb03>();
  g_results[3] = read_counter<0xb04>();
  g_results[4] = read_counter<0xb05>();
  g_results[5] = read_counter<0xb06>();
  return 0;
}
