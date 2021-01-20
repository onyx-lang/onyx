#ifndef ONYXDOC_H
#define ONYXDOC_H

#include "bh.h"
#include "onyxastnodes.h"

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
void onyx_docs_emit(OnyxDocumentation* doc);

#endif
