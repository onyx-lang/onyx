#ifndef ONYXDOC_H
#define ONYXDOC_H

#include "bh.h"
#include "astnodes.h"

// Onyx Documentation generation

void onyx_docs_emit_odoc(const char *dest);


// Tag generation

void onyx_docs_emit_tags(char *dest);



// Symbol info file generation

typedef struct SymbolInfo SymbolInfo;
struct SymbolInfo {
	u32 id;
	u32 file_id;
	u32 line;
	u32 column;
};

typedef struct SymbolResolution SymbolResolution;
struct SymbolResolution {
	u32 file_id;
	u32 line;
	u32 column;
	u32 length;
	u32 symbol_id;
};

typedef struct SymbolInfoTable SymbolInfoTable;
struct SymbolInfoTable {
	// File name -> file id
	Table(u32) files;
	u32 next_file_id;

	// Symbol id -> symbol info
	bh_arr(SymbolInfo) symbols;
	u32 next_symbol_id;

	bh_arr(SymbolResolution) symbols_resolutions;

	// Node * -> symbol_id
	bh_imap node_to_id;
};

void onyx_docs_emit_symbol_info(const char *dest);

#endif
