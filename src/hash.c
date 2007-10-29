#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "sha1.h"

SEXP hash_object(SEXP object)
{
	char output[41];  /* SHA-1 is 40 bytes + '\0' */
	char *data;
	int nChar, i;
	SEXP result;
	sha1_context ctx;
	unsigned char buffer[20];

	data = (char *) RAW(object);
	nChar = LENGTH(object);
	
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


