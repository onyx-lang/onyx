#ifndef ONYXDOC_H
#define ONYXDOC_H

#include "bh.h"
#include "astnodes.h"

typedef enum DocFormat {
    Doc_Format_Human,
    Doc_Format_Tags,
    Doc_Format_Html,
} DocFormat;

typedef struct DocEntry {
	OnyxFilePos pos;
    char* sym; // Unused by doc generator
	char* def;
	char* additional;
} DocEntry;

typedef struct DocPackage {
	const char* name;

	bh_arr(DocEntry) public_entries;
	bh_arr(DocEntry) private_entries;
} DocPackage;

typedef struct OnyxDocumentation {
	bh_arena doc_arena;

    DocFormat format;

	bh_arr(DocPackage) package_docs;
} OnyxDocumentation;

OnyxDocumentation onyx_docs_generate();
void onyx_docs_emit(OnyxDocumentation* doc, const char* filename);


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
