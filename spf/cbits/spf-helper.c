#include <netinet/in.h>
#include <spf2/spf.h>

typedef enum RequestResult {
  SPF_REQUEST_ALLOC_FAIL,
  SPF_REQUEST_INVALID_IP,
  SPF_REQUEST_INVALID_HELO_DOMAIN,
  SPF_REQUEST_INVALID_ENVELOPE_FROM,
  SPF_REQUEST_RESULT_INVALID,
  SPF_REQUEST_RESULT_NEUTRAL,
  SPF_REQUEST_RESULT_PASS,
  SPF_REQUEST_RESULT_FAIL,
  SPF_REQUEST_RESULT_SOFTFAIL,
  SPF_REQUEST_RESULT_NONE,
  SPF_REQUEST_RESULT_TEMPERROR,
  SPF_REQUEST_RESULT_PERMERROR
} RequestResult;

RequestResult spf_make_request(
  SPF_server_t *spf_server,
  char *opt_ip,
  char *opt_helo,
  char *opt_sender
  ){
  SPF_request_t *spf_request = SPF_request_new(spf_server);
  if (spf_request == NULL) {
    return SPF_REQUEST_ALLOC_FAIL;
  }
  if ( SPF_request_set_ipv4_str( spf_request, opt_ip ) ) {
    return SPF_REQUEST_INVALID_IP;
  }
  if ( SPF_request_set_helo_dom( spf_request, opt_helo ) ) {
    return SPF_REQUEST_INVALID_HELO_DOMAIN;
  }
  if ( SPF_request_set_env_from( spf_request, opt_sender ) ) {
    return SPF_REQUEST_INVALID_ENVELOPE_FROM;
  }
  SPF_response_t *spf_response = NULL;
  SPF_request_query_mailfrom(spf_request, &spf_response);
  SPF_result_t result = SPF_response_result(spf_response);
  SPF_response_free(spf_response);
  SPF_request_free(spf_request);
  switch (result) {
  case SPF_RESULT_NEUTRAL   : return SPF_REQUEST_RESULT_NEUTRAL;
  case SPF_RESULT_PASS      : return SPF_REQUEST_RESULT_PASS;
  case SPF_RESULT_FAIL      : return SPF_REQUEST_RESULT_FAIL;
  case SPF_RESULT_SOFTFAIL  : return SPF_REQUEST_RESULT_SOFTFAIL;
  case SPF_RESULT_NONE      : return SPF_REQUEST_RESULT_NONE;
  case SPF_RESULT_TEMPERROR : return SPF_REQUEST_RESULT_TEMPERROR;
  case SPF_RESULT_PERMERROR : return SPF_REQUEST_RESULT_PERMERROR;
  case SPF_RESULT_INVALID   : return SPF_REQUEST_RESULT_INVALID;
  default:
    return SPF_REQUEST_RESULT_INVALID;
  }
}
