// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved
// Copyright (c) 2022 Ashwin Menon. All Rights Reserved
// Made more generic.

// This program reads an ELF file and outputs a Verilog hex memory
// image file (suitable for reading using $readmemh).

// ================================================================
#include <algorithm>
#include <cstdint>
#include <fcntl.h>
#include <fstream>
#include <gelf.h>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

/* An aligned data block with leading and trailing 0-padding so that its start
   address and size are aligned to the 'alignment' parameter.

   E.g., with alignment=8, we get a data block whose start address is 8-byte
   aligned and whose size is also 8-byte aligned.
*/
class AlignedDataBlock {
public:
  AlignedDataBlock() {
    start_ = 0;
    data_ = std::vector<uint8_t>(0);
  }

  AlignedDataBlock(uint16_t alignment, uint64_t addr, const uint8_t *data,
                   uint64_t size) {
    uint64_t addr_mask_ = (~((uint64_t)(alignment - 1)));
    start_ = addr & addr_mask_;
    alignment_ = alignment;

    // Add the leading padding
    data_ = std::vector<uint8_t>(addr % alignment, 0);

    // Add the data
    data_.insert(data_.end(), data, &data[size]);

    // Add the trailing padding
    if (data_.size() % alignment)
      data_.insert(data_.end(), (alignment - data_.size() % alignment), 0);
  }

  uint64_t Addr() const { return start_; }

  std::vector<uint8_t>::size_type Size() const { return data_.size(); }

  uint64_t End() const { return start_ + data_.size(); }

  uint8_t Data(std::vector<uint8_t>::size_type i) const { return data_[i]; }

  bool Overlaps(uint64_t addr) const {
    // Empty blocks never overlap any address
    if (!Size())
      return false;
    else
      return addr >= start_ && addr < End();
  }

  void Join(uint64_t addr, const uint8_t *data, uint64_t size) {
    if (!Overlaps(addr))
      return;

    // Expand the data storage if required
    if ((addr + size) > End())
      data_.resize(data_.size() + addr + size - End(), 0);

    // Add trailing padding if required
    if (data_.size() % alignment_)
      data_.insert(data_.end(), (alignment_ - data_.size() % alignment_), 0);

    // Insert the new data
    for (std::vector<uint8_t>::size_type i = addr - start_, j = 0; j < size;
         i++, j++)
      data_[i] = data[j];
  }

private:
  uint64_t start_;
  uint16_t alignment_;
  std::vector<uint8_t> data_;
};

class HexFile {
public:
  HexFile(const std::string &fname, uint16_t alignment, uint64_t mem_size,
          uint64_t base_addr)
      : alignment_(alignment), base_addr_(base_addr), mem_size_(mem_size) {
    of_ = std::ofstream(fname);
  }

  void PutBlock(uint64_t addr, const uint8_t *data, uint64_t size) {
    if (!size)
      return;
    // if the new block starts within the current one, then join it to the
    // current one; otherwise start a new one
    if (curr_block_.Overlaps(addr)) {
      curr_block_.Join(addr, data, size);
    } else {
      Write(curr_block_);
      curr_block_ = AlignedDataBlock(alignment_, addr, data, size);
    }
  }

  void Close() {
    Write(curr_block_);

    /* There is a bug in Verilator that raises an error if a readmem file does
      not have an entry for the last address. So we put in a last address entry
      in the hex file to avoid this.
      Note: this bug is fixed in Verilator version 4.218
      See: https://github.com/verilator/verilator/issues/3205
    */
    uint64_t last_block_addr = base_addr_ + mem_size_ - alignment_;
    if (!curr_block_.Overlaps(last_block_addr)) {
      std::vector<uint8_t> last_data(alignment_, 0);
      curr_block_ = AlignedDataBlock(alignment_, last_block_addr,
                                     last_data.data(), alignment_);
      Write(curr_block_);
    }
    of_.close();
  }

  void Write(const AlignedDataBlock &block) {
    // Don't write empty blocks
    if (!block.Size())
      return;

    // Write the address
    of_ << "@" << std::hex << std::setw(7) << std::setfill('0') << std::right
        << (block.Addr() - base_addr_) / alignment_
        << "    // raw_mem addr;  byte addr: " << std::setw(8)
        << block.Addr() - base_addr_ << std::endl;

    // Write the data
    for (uint64_t i = 0; i < block.Size(); i += alignment_) {
      for (int32_t j = alignment_ - 1; j >= 0; j--) {
        of_ << std::hex << std::setw(2) << std::setfill('0') << std::right
            << (uint32_t)block.Data(i + j);
      }
      of_ << "    // raw_mem addr " << std::setw(8) << std::right
          << (block.Addr() + i - base_addr_) / alignment_ << ";  byte addr "
          << std::setw(8) << std::right << (block.Addr() + i - base_addr_)
          << std::endl;
    }
  }

private:
  std::ofstream of_;
  AlignedDataBlock curr_block_;
  uint16_t alignment_;
  uint64_t base_addr_;
  uint64_t mem_size_;
};

// Memory ranges read from the ELF file
std::map<uint64_t, std::vector<uint8_t>> mem_ranges;

// Features of the ELF binary
int bitwidth;
uint64_t min_addr;
uint64_t max_addr;

uint64_t pc_start;    // Addr of label  '_start'
uint64_t pc_exit;     // Addr of label  'exit'
uint64_t tohost_addr; // Addr of label  'tohost'

// ================================================================
// Load an ELF file.

void mem_load_elf(const std::string &elf_filename, std::string start_symbol,
                  std::string exit_symbol, std::string tohost_symbol) {
  int fd;
  // int n_initialized = 0;
  Elf *e;

  // Default start, exit and tohost symbols
  if (start_symbol == "")
    start_symbol = "_start";
  if (exit_symbol == "")
    exit_symbol = "exit";
  if (tohost_symbol == "")
    tohost_symbol = "tohost";

  // Verify the elf library version
  if (elf_version(EV_CURRENT) == EV_NONE) {
    std::cerr
        << "ERROR: c_mem_load_elf: Failed to initialize the libelfg library!"
        << std::endl;
    exit(1);
  }

  // Open the file for reading
  fd = open(elf_filename.c_str(), O_RDONLY, 0);
  if (fd < 0) {
    std::cerr << "ERROR: c_mem_load_elf: could not open elf input file: "
              << elf_filename << std::endl;
    exit(1);
  }

  // Initialize the Elf pointer with the open file
  e = elf_begin(fd, ELF_C_READ, NULL);
  if (e == NULL) {
    std::cerr << "ERROR: c_mem_load_elf: elf_begin() initialization failed!"
              << std::endl;
    exit(1);
  }

  // Verify that the file is an ELF file
  if (elf_kind(e) != ELF_K_ELF) {
    elf_end(e);
    std::cerr
        << "ERROR: c_mem_load_elf: specified file '%s' is not an ELF file!"
        << elf_filename << std::endl;
    exit(1);
  }

  // Get the ELF header
  GElf_Ehdr ehdr;
  if (gelf_getehdr(e, &ehdr) == NULL) {
    elf_end(e);
    std::cerr << "ERROR: c_mem_load_elf: get_getehdr() failed: "
              << elf_errmsg(-1) << std::endl;
    exit(1);
  }

  // Is this a 32b or 64 ELF?
  if (gelf_getclass(e) == ELFCLASS32) {
    std::cout << "c_mem_load_elf: " << elf_filename << " is a 32-bit ELF file"
              << std::endl;
    bitwidth = 32;
  } else if (gelf_getclass(e) == ELFCLASS64) {
    std::cout << "c_mem_load_elf: " << elf_filename << " is a 64-bit ELF file"
              << std::endl;
    bitwidth = 64;
  } else {
    std::cerr << "c_mem_load_elf: ELF File " << elf_filename
              << " is not 32b or 64b" << std::endl;
    elf_end(e);
    exit(1);
  }

  // Verify we are dealing with a RISC-V ELF
  if (ehdr.e_machine != 243) { // EM_RISCV is not defined, but this returns 243
                               // when used with a valid elf file.
    elf_end(e);
    std::cerr << "ERROR: c_mem_load_elf: " << elf_filename
              << " is not a RISC-V ELF file" << std::endl;
    exit(1);
  }

  // Verify we are dealing with a little endian ELF
  if (ehdr.e_ident[EI_DATA] != ELFDATA2LSB) {
    elf_end(e);
    std::cerr
        << "ERROR: c_mem_load_elf: " << elf_filename
        << " is a big-endian 64-bit RISC-V executable which is not supported"
        << std::endl;
    exit(1);
  }

  // Grab the string section index
  size_t shstrndx;
  shstrndx = ehdr.e_shstrndx;

  // Iterate through each of the sections looking for code that should be loaded
  Elf_Scn *scn = 0;
  GElf_Shdr shdr;

  min_addr = 0xFFFFFFFFFFFFFFFFllu;
  max_addr = 0x0000000000000000llu;
  pc_start = 0xFFFFFFFFFFFFFFFFllu;
  pc_exit = 0xFFFFFFFFFFFFFFFFllu;
  tohost_addr = 0xFFFFFFFFFFFFFFFFllu;

  while ((scn = elf_nextscn(e, scn)) != NULL) {
    // get the header information for this section
    gelf_getshdr(scn, &shdr);

    char *sec_name = elf_strptr(e, shstrndx, shdr.sh_name);
    std::cout << "Section " << std::setw(16) << sec_name << ":";

    Elf_Data *data = 0;
    // If we find a code/data section, load it into the model
    if (((shdr.sh_type == SHT_PROGBITS) || (shdr.sh_type == SHT_NOBITS) ||
         (shdr.sh_type == SHT_INIT_ARRAY) ||
         (shdr.sh_type == SHT_FINI_ARRAY)) &&
        ((shdr.sh_flags & SHF_WRITE) || (shdr.sh_flags & SHF_ALLOC) ||
         (shdr.sh_flags & SHF_EXECINSTR))) {
      data = elf_getdata(scn, data);

      // n_initialized += data->d_size;
      if (shdr.sh_addr < min_addr)
        min_addr = shdr.sh_addr;
      if (max_addr < (shdr.sh_addr + data->d_size - 1)) // shdr.sh_size + 4))
        max_addr = shdr.sh_addr + data->d_size - 1;     // shdr.sh_size + 4;

      if (shdr.sh_type != SHT_NOBITS) {
        uint8_t *dbuf = reinterpret_cast<uint8_t *>(data->d_buf);
        mem_ranges[shdr.sh_addr] =
            std::vector<uint8_t>(dbuf, &dbuf[data->d_size]);
      }
      std::cout << std::showbase << std::hex << "addr " << std::setw(16)
                << shdr.sh_addr << " to addr " << std::setw(16)
                << shdr.sh_addr + data->d_size << "; size " << std::setw(8)
                << data->d_size << " (=" << std::dec << data->d_size
                << ") bytes" << std::noshowbase << std::endl;

    }

    // If we find the symbol table, search for symbols of interest
    else if (shdr.sh_type == SHT_SYMTAB) {
      std::cout << "Searching for addresses of '" << start_symbol << "', '"
                << exit_symbol << "' and '" << tohost_symbol << "'"
                << " symbols" << std::endl;

      // Get the section data
      data = elf_getdata(scn, data);

      // Get the number of symbols in this section
      int symbols = shdr.sh_size / shdr.sh_entsize;

      // search for the uart_default symbols we need to potentially modify.
      GElf_Sym sym;
      int i;
      for (i = 0; i < symbols; ++i) {
        // get the symbol data
        gelf_getsym(data, i, &sym);

        // get the name of the symbol
        char *name = elf_strptr(e, shdr.sh_link, sym.st_name);

        // Look for, and remember PC of the start symbol
        if (name == start_symbol) {
          pc_start = sym.st_value;
        }
        // Look for, and remember PC of the exit symbol
        else if (name == exit_symbol) {
          pc_exit = sym.st_value;
        }
        // Look for, and remember addr of 'tohost' symbol
        else if (name == tohost_symbol) {
          tohost_addr = sym.st_value;
        }
      }

      std::ofstream of("symbol_table.txt");
      of << "Writing symbols to:    symbol_table.txt" << std::endl;
      if (pc_start == -1)
        std::cout << "    No '_start' label found" << std::endl;
      else
        of << std::showbase << "_start    " << std::hex << pc_start
           << std::endl;
      if (pc_exit == -1)
        std::cout << "    No 'exit' label found" << std::endl;
      else
        of << std::showbase << "exit    " << std::hex << pc_exit << std::endl;
      if (tohost_addr == -1)
        std::cout << "    No 'tohost' label found" << std::endl;
      else
        of << std::showbase << "tohost    " << std::hex << tohost_addr
           << std::endl;
      of.close();
    } else {
      std::cout << "Ignored" << std::endl;
    }
  }

  elf_end(e);

  std::cout << std::showbase << std::hex
            << "Min addr:            " << std::setw(16) << min_addr
            << std::endl;
  std::cout << "Max addr:            " << std::setw(16) << max_addr
            << std::endl;
  std::cout << std::noshowbase << std::dec;
}

// ================================================================

void print_usage(int argc, char *argv[]) {
  std::cerr
      << "Usage:" << std::endl
      << "    " << argv[0] << "  --help" << std::endl
      << "    " << argv[0] << "  <Hex memory base address> "
      << "  <Hex memory size> "
      << "<ELF filename>  <mem hex filename>" << std::endl
      << "Reads ELF file and writes a Verilog Hex Memory image file"
      << std::endl
      << "Note: the hex numbers are in BSV unsized format (E.g., 'h2_000)."
      << std::endl
      << "Example: To create Mem.hex from main.elf for a "
      << "256MB memory located at 0x80000000:" << std::endl
      << "    " << argv[0] << " 'h8000_0000 'h1000_0000 main.elf Mem.hex"
      << std::endl;
}

// ================================================================

std::pair<uint64_t, bool> ParseHex(std::string s) {
  uint64_t ret = 0;
  bool valid = false;

  if (s.length() > 2 && s[0] == '\'' && (s[1] == 'h' || s[1] == 'H')) {
    // strip the initial "'h"
    s = s.substr(2);
    // remove underscores
    std::string s2;
    for (auto c : s) {
      if (c != '_')
        s2.push_back(c);
    }
    ret = std::stoll(s2, nullptr, 16);
    valid = true;
  }
  return {ret, valid};
}

// ================================================================

int main(int argc, char *argv[]) {
  uint16_t alignment = 32;

  if ((argc == 2) && (std::string(argv[1]) == "--help")) {
    print_usage(argc, argv);
    return 0;
  } else if (argc != 5) {
    print_usage(argc, argv);
    return 1;
  }

  uint64_t base_addr;
  uint64_t mem_size;
  auto a = ParseHex(argv[1]);
  if (!a.second) {
    std::cerr << "Bad hex number: " << argv[1] << std::endl;
    print_usage(argc, argv);
    exit(1);
  }
  base_addr = a.first;
  a = ParseHex(argv[2]);
  if (!a.second) {
    std::cerr << "Bad hex number: " << argv[2] << std::endl;
    print_usage(argc, argv);
    exit(1);
  }
  mem_size = a.first;

  std::cout << std::hex << std::showbase
            << "Making HEX file for memory at base address " << base_addr
            << " of size " << mem_size << std::noshowbase << std::endl;

  mem_load_elf(argv[3], "_start", "exit", "tohost");

  if (min_addr < base_addr) {
    std::cerr << std::hex << std::showbase << "ERROR: The minimum address ("
              << min_addr << ") "
              << "is less than the base address given (" << base_addr << ")"
              << std::noshowbase << std::endl;
    print_usage(argc, argv);
    exit(1);
  }

  if (max_addr >= base_addr + mem_size) {
    std::cerr << std::hex << std::showbase << "ERROR: The maximum address ("
              << max_addr << ") "
              << "is greater than the memory size given (" << mem_size << ")"
              << std::noshowbase << std::endl;
    print_usage(argc, argv);
    exit(1);
  }

  // Sort the mem range addresses in ascending order
  std::vector<uint64_t> addresses;
  for_each(mem_ranges.begin(), mem_ranges.end(),
           [&addresses](const std::pair<uint64_t, std::vector<uint8_t>> &r) {
             addresses.push_back(r.first);
           });
  std::sort(addresses.begin(), addresses.end());

  // Put the mem ranges into the hex file
  HexFile hf(argv[4], alignment, mem_size, base_addr);
  for_each(addresses.begin(), addresses.end(), [&hf](const uint64_t addr) {
    hf.PutBlock(addr, mem_ranges[addr].data(), mem_ranges[addr].size());
  });

  // Close the hex file
  hf.Close();
}