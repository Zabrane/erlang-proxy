-define(ADDR_IPV4, 16#01).
-define(ADDR_IPV6, 16#04).
-define(ADDR_DOMAIN, 16#03).

-define(AUTHMAP,[{default,16#00},
                 {no_auth,16#00},
                 {user_pass,16#02}
                ]).

-define(CMD_CONNECT,16#01).
-define(CMD_BIND,16#02).
-define(CMD_UDPASSOCIATE,16#03).
  
