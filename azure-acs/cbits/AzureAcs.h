#ifndef __AZURE_ACS__
#define __AZURE_ACS__

struct azureACS_info_str{
  const char *relyingParty;
  const char *issuerName;
  const char *key;
  const char *acsUrl;
};

typedef struct azureACS_info_str *azureACS_info;
typedef void *azureACS_context;
typedef void *azureACS_token;


void azureACS_initialize();
azureACS_context azureACS_getContext(azureACS_info);
azureACS_token azureACS_getToken(azureACS_context context);


const char *azureACS_wrapAccessToken(azureACS_token token);
long azureACS_validForSeconds(azureACS_token token);

void azureACS_finalizeContext(azureACS_context context);
void azureACS_finalizeToken(azureACS_token token); 
void azureACS_printError(azureACS_context context);
int azureACS_isValid(azureACS_context context);
int azureACS_tokenStatus(azureACS_token token);
char *azureACS_tokenError(azureACS_token token);
#endif
