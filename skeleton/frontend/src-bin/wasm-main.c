#include <stdlib.h>
#include <stdio.h>
#include "HsFFI.h"
#include <unistd.h>
#include "Rts.h"

// Copied from "stub.h" file

extern HsStablePtr hsAppInit(void);
extern HsInt64 hsJsaddleProcessResult(HsStablePtr a1, HsPtr a2, HsBool a3, HsInt a4);
extern HsPtr hsMalloc(HsInt a1);

HsStablePtr hsEnvPtr = NULL;

char* jsaddleBuffer = NULL;

char* jsaddleBufferMalloc(int size) {
  if (size == 0) {
    return NULL;
  }
  if (jsaddleBuffer) { return jsaddleBuffer;}
  jsaddleBuffer = (char*) hsMalloc(size);
  return jsaddleBuffer;
}

int jsaddleProcessResult (bool isSync, int dataLen) {
  int ret = hsJsaddleProcessResult (hsEnvPtr, jsaddleBuffer, isSync, dataLen);
  return ret;
}

HsBool mylib_init(void){
  int argc = 0;
  char *argv[] = { NULL };
  char **pargv = argv;

  RtsConfig conf = defaultRtsConfig;
  /* conf.rts_opts = "-DS -DG"; */
  conf.keep_cafs = true;
  hs_init_ghc(&argc, &pargv, conf);

  hsEnvPtr = hsAppInit();

  return HS_BOOL_TRUE;
}

int main(int argc, char * argv[]) {
  mylib_init();
  printf("Haskell initialization done");
  _Exit(0);
}
