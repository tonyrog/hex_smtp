%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Hex SMTP plugin 
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_smtp).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal(), Cb::function()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(_Flags, _Signal, _Cb) ->
    {error, no_input}.

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(_Ref) ->
    {error, no_input}.

%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags0, Env) ->
    Account = proplists:get_value(account, Flags0),
    {ok,AccountFlags} = application:get_env(hex_smtp, Account),
    Flags = proplists:delete(account,Flags0) ++ AccountFlags,
    From = proplists:get_value(from, Flags),
    {ok,[{_Name,Sender}]} = smtp_util:parse_rfc822_addresses(From),
    To = proplists:get_value(to, Flags),
    {ok,Recipients0} = smtp_util:parse_rfc822_addresses(To),
    Recipients = [R || {_,R} <- Recipients0],

    {Type,SubType,Parts} =
	case proplists:get_value(body, Flags) of
	    {text,Body} when is_list(Body); is_binary(Body) -> 
		Utf8 = to_utf8(hex:text_expand(Body, Env)),
		{<<"text">>,<<"plain">>,Utf8};
	    {image,IType,File} ->
		{ok,Bin} = file:read_file(File),
		{<<"image">>,to_binary(IType),Bin};
	    Body when is_list(Body); is_binary(Body) ->
		Utf8 = to_utf8(hex:text_expand(Body, Env)),
		{<<"text">>,<<"plain">>,Utf8}
	end,
    
    Headers = [{<<"From">>, From}, {<<"To">>, To}] ++
	get_header(subject, <<"Subject">>, Flags, []) ++
	get_gen_header(date, <<"Date">>, Flags, 
		       fun() -> smtp_util:rfc5322_timestamp() end) ++
	get_gen_header(message_id, <<"Message-ID">>, Flags, 
		       fun() -> smtp_util:generate_message_id() end) ++
	[],

    ContentTypeParams = 
	[{<<"content-type-params">>, 
	  [{<<"charset">>,<<"UTF-8">>}],
	  {<<"disposition">>,<<"inline">>}}],

    Message = mimemail:encode({Type,SubType,Headers,
			       ContentTypeParams, Parts}),
    Flags1 = lists:foldl(
	       fun(F,Fs) -> proplists:delete(F, Fs) end, 
	       Flags,
	       [from,to,subject,body,date,message_id]),

    gen_smtp_client:send({Sender, Recipients, Message}, Flags1, 
			 fun({ok,Recipient}) ->
				 lager:debug("message delivered to ~s",
					     [Recipient]);
			    (Error) ->
				 lager:error("message delivery error ~p",
					     [Error])
			 end).

%%
%% init_event(in | out, Flags::[{atom(),term()}])
%%
init_event(_, _) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(in, _Flags) ->
    {error, no_input};
validate_event(out, Flags) ->
    hex:validate_flags(Flags,output_spec()).

output_spec() ->
    AccountSpec = 
	[{relay, mandatory, fun inet_parse:domain/1, ""},
	 {port, optional,  {integer,1,65535}, undefined},
	 {ssl, optional, boolean, false},
	 {username, optional, string, undefined},
	 {password, optional, string, undefined},
	 {tls,optional,{alt,[{const,if_available},
			     {const,always},
			     {const,never}]}, if_available},
	 {auth,optional,{alt,[{const,if_available},
			      {const,always},
			      {const,never}]}, if_available},
	 {retries,optional,{integer,1,1000}, 1},
	 {date,optional,{alt,[boolean,string]},false},
	 {message_id,optional,{alt,[boolean,string]},false}],
    [
     {account,mandatory,{sys_config,hex_smtp,AccountSpec},undefined},
     {from, mandatory, fun is_rfc822/1, undefined },
     {to,   mandatory, fun is_rfc822_list/1, undefined },
     {subject, optional, string, "no subject"},
     {body,  mandatory, 
      %% {text, Body::string()} |
      %% {image, IType::string(), File::string()}
      %% Body::string()
      %% FIXME: add explicit unicode type!
      {alt, [{tuple, [{const,text}, string]},
	     {tuple, [{const,image},string, string]},
	     string]}, undefined},
     {date,optional,{alt,[boolean,string]},false},
     {message_id,optional,{alt,[boolean,string]},false}
    ].

is_rfc822(Value) ->
    try smtp_util:parse_rfc822_addresses(Value) of
	{ok,[_]} -> true;
	_ -> false
    catch
	error:_ -> false
    end.

is_rfc822_list(Value) ->
    try smtp_util:parse_rfc822_addresses(Value) of
	{ok,[_|_]} -> true;
	_ -> false
    catch
	error:_ -> false
    end.


%% get_header(Key, Field, Flags) ->
%%    case proplists:get_value(Key, Flags) of
%%	undefined -> exit({missing_field, Key});
%%	Value -> [{Field, to_binary(Value)}]
%%    end.

get_header(Key, Field, Flags, Default) ->
    case proplists:get_value(Key, Flags) of
	undefined -> Default;
	Value -> [{Field, to_binary(Value)}]
    end.

get_gen_header(Key, Field, Flags, Gen) ->
    case proplists:get_value(Key, Flags, true) of
	true ->
	    [{Field, to_binary(Gen())}];
	false ->
	    [];
	Value -> [{Field, to_binary(Value)}]
    end.

to_binary(Data) -> erlang:iolist_to_binary(Data).

to_utf8(Data) ->
    unicode:characters_to_binary(Data).

