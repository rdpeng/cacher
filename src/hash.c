#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "sha1.h"

/* 
 * This code is adapted from the 'digest.c' code in the 'digest'
 * package
 */

SEXP sha1_object(SEXP object, SEXP skip_bytes)
{
	char output[41];  /* SHA-1 is 40 bytes + '\0' */
	Rbyte *data;
	int nChar, i, skip;
	SEXP result;
	sha1_context ctx;
	unsigned char buffer[20];

	data = RAW(object);
	nChar = length(object);
	skip = INTEGER(skip_bytes)[0];
	
	if(skip > 0) {
		if(skip >= nChar)
			nChar = 0;
		else {
			nChar -= skip;
			data += skip;
		}
	}
	sha1_starts(&ctx);
	sha1_update(&ctx, (uint8 *) data, nChar);
	sha1_finish(&ctx, buffer);

	for(i=0; i < 20; i++) {
		sprintf(output + i * 2, "%02x", buffer[i]);
	}
	PROTECT(result = allocVector(STRSXP, 1));
	SET_STRING_ELT(result, 0, mkChar(output));
	UNPROTECT(1);

	return result;
}


