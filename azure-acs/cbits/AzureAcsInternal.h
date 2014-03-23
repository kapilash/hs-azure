#ifndef __AZURE_ACS_INT__
#define __AZURE_ACS_INT__

#include <curl/curl.h>
#include "AzureAcs.h"


struct azureACS_token_str{
  char *token;
  size_t length;
  long  seconds;
  int status;
  char *error;
};

struct azureACS_credentials_str {
  unsigned char *relyingParty;
  unsigned char *issuerName;
  unsigned char *key;
  CURL *curl;
  struct azureACS_token_str *response;
  struct curl_slist *headerList;
  char *errorBuffer;
  char *buffer;
  unsigned char isValid;
};


typedef struct azureACS_credentials_str *azureACS_credentials;
typedef struct azureACS_token_str *TokenData;

#endif
