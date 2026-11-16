#ifndef ONYXDOC_H
#define ONYXDOC_H

#include "bh.h"
#include "astnodes.h"

// Onyx Documentation generation

void onyx_docs_submit(OnyxDocInfo *docs, AstBinding *binding);
void onyx_docs_generate_odoc(Context *context, bh_buffer *out_buffer);



// Symbol info file generation

typedef struct SymbolInfo SymbolInfo;
struct SymbolInfo {
	u32 id;
	u32 file_id;
	u32 line;
	u32 column;
	const char *documentation;
	u32 documentation_length;
};

typedef struct SymbolResolution SymbolResolution;
struct SymbolResolution {
	u32 file_id;
	u32 line;
	u32 column;
	u32 length;
	u32 symbol_id;
};

void onyx_docs_emit_symbol_info(Context *context, bh_buffer *out_buffer);

#endif
