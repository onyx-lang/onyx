#ifndef ONYXDOC_H
#define ONYXDOC_H

#include "bh.h"
#include "onyxastnodes.h"

typedef struct DocEntry {
	OnyxFilePos pos;
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

	bh_arr(DocPackage) package_docs;
} OnyxDocumentation;

OnyxDocumentation onyx_docs_generate(ProgramInfo* prog);
void onyx_docs_write(OnyxDocumentation* doc);

#endif