-module(protocol_socks5).

-compile([export_all]).

-include("priv/socks5_define.hrl").

parse_protocol(init,Buf) when erlang:size(Buf) =< 2->
    {init,undefined,Buf};
parse_protocol(init,<<16#05:8,NMethods:8,Payload/binary>>) when NMethods =< erlang:size(Payload)->
    <<Methods:NMethods/binary,Rest/bits >> = Payload,
    {request,{auth,Methods},Rest};

parse_protocol(request,Buf) when erlang:size(Buf) =< 4->
    {request,undefined,Buf};

parse_protocol(request,<<16#05:8, CmdCode:8, 0:8, AddrType:8, Payload/binary>> = Buf)->
    case CmdCode of
        ?CMD_CONNECT ->
            case parse_addr_type(AddrType,Payload) of
                {ok,Command,Rest}->
                    {process,Command,Rest};
                {error,_}->
                    {request,undefined,Buf}
             end;
        ?CMD_BIND ->
            {process,stop,Buf};
        ?CMD_UDPASSOCIATE ->
            {process,stop,Buf}
    end;

parse_protocol(Step,Buf)->
    {Step,undefined,Buf}.

parse_addr_type(?ADDR_IPV4,Payload)->
    Size = erlang:size(Payload),
    case Size >= 6 of
        true ->
            <<Address:32, Port:16,Rest/bits>> = Payload,
            {ok,{connect,{ipv4,Address,Port}},Rest};
        _ ->
            {error,Payload}
    end;
parse_addr_type(?ADDR_IPV6,Payload) ->
    Size = erlang:size(Payload),
    case Size >= 18 of
        true ->
            <<Address:128, Port:16,Rest/bits>> = Payload,
            {ok,{connect,{ipv4,Address,Port}},Rest};
        _ ->
            {error,Payload}
    end;
parse_addr_type(?ADDR_DOMAIN,Payload)->
    Size = erlang:size(Payload),
    case Size > 1 of
        true ->
            <<DomainLen:8, Other/binary>> = Payload,
            OtherSize = erlang:size(Other) - 2,
            case OtherSize >= DomainLen of
                true ->
                    <<DomainBin:DomainLen/binary, Port:16,Rest/bits>> = Other,
                    {ok,{connect, {domain, DomainBin, Port}},Rest};
                _->
                    {error,Payload}
            end;
         _->
            {error,Payload}
    end.

build_protocol(auth,AuthType)->
    Code = case AuthType of 
              undefined -> 
                   proplists:get_value(default,?AUTHMAP);
               _ ->
                   proplists:get_value(AuthType,?AUTHMAP)
           end,
    case Code of
        udefined ->
            DefaultCode = proplists:get_value(default,?AUTHMAP),
            <<16#05:8,DefaultCode>>;
        _ ->
            <<16#05:8,Code>>
    end;
build_protocol(process,{granted, {ipv4, IP, Port}}) ->
    <<16#05:8, 16#00:8, 0:8, ?ADDR_IPV4, IP/binary, Port:16>>;
build_protocol(process,{granted, {ipv6, IP, Port}}) ->
    <<16#05:8, 16#00:8, 0:8, ?ADDR_IPV6, IP/binary, Port:16>>;
build_protocol(process,{granted, {domain, DomainBin, Port}}) ->
    <<16#05:8, 16#00:8, 0:8, ?ADDR_DOMAIN, (byte_size(DomainBin)):8, DomainBin/binary, Port:16>>;
build_protocol(process,{rejected,_}) ->
    <<16#05:8, 16#02:8, 0:8>>.
