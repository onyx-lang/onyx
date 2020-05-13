#include "bh.h"

#include "onyxlex.h"

typedef struct OnyxParser {
	OnyxTokenizer tokenizer;
	OnyxToken *prev;
	OnyxToken *curr;

	bh_hash(OnyxIdentifier) idens;

	bh_arr(OnyxToken) tokens; /* Maybe don't store the whole array? Ask for one as you need it? */
} OnyxParser;

typedef struct OnyxParseNode {
	OnyxToken *token;

};