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
	 add_event/2, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(_Flags, _Signal) ->
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
output(Flags0, _Env) ->
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
	    {text,Body} -> 
		{<<"text">>,<<"plain">>,to_binary(Body)};
	    {image,IType,File} ->
		{ok,Bin} = file:read_file(File),
		{<<"image">>,to_binary(IType),Bin};
	    Body when is_list(Body); is_binary(Body) ->
		{<<"text">>,<<"plain">>,to_binary(Body)}
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
	  [{<<"charset">>,<<"US-ASCII">>}],
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
    try validate_out_event(Flags) of
	ok -> ok;
	Error -> Error
    catch
	throw:{error,Reason} ->
	    {error,Reason}
    end.

%% account, from, to, body, subject, date, message_id
validate_out_event(Flags0) ->
    Account = get_mandatory(account, Flags0, fun erlang:is_atom/1),
    Flags = case application:get_env(hex_smtp,Account) of
		{ok,AccountFlags} -> 
		    validate_account(AccountFlags),
		    Flags0 ++ AccountFlags;
		_ -> 
		    throw({error, {missing_sys_config,[{hex_smtp,Account}]}})
	    end,
    get_mandatory(from, Flags, fun(V) -> is_rfc822(V) end),
    get_mandatory(to, Flags, fun(V) -> is_rfc822_list(V) end),
    get_mandatory(body, Flags, fun(V) -> is_body(V) end),
    get_optional(date, Flags, false, fun is_boolean_or_string/1),    
    get_optional(message_id, Flags, false, fun is_boolean_or_string/1),
    ok.

%% relay, ssl, auth, username, password, tls, port, from, date, message_id
validate_account(Flags) ->
    get_mandatory(relay, Flags, fun inet_parse:domain/1),
    get_optional(port, Flags, undefined, fun is_port_number/1),
    get_optional(ssl, Flags, false, fun erlang:is_boolean/1),
    get_optional(username, Flags, undefined, fun is_string/1),
    get_optional(password, Flags, undefined, fun is_string/1),
    get_optional(tls, Flags, if_available, fun is_tls/1),
    get_optional(auth, Flags, if_available, fun is_auth/1),
    get_optional(retries, Flags, 1, fun is_positive_number/1),
    get_optional(date, Flags, false, fun is_boolean_or_string/1),    
    get_optional(message_id, Flags, false, fun is_boolean_or_string/1).
    

get_mandatory(Key, List, Validate) ->
    case proplists:lookup(Key,List) of
	none -> throw({error, {mandatory,[Key]}});
	{_,Value} -> 
	    try Validate(Value) of
		false -> throw({error, {badarg,[Key]}});
		true -> Value
	    catch
		error:_ ->
		    throw({error, {badarg,[Key]}})
	    end
    end.

get_optional(Key, List, Default, Validate) ->
    case proplists:lookup(Key,List) of
	none -> Default;
	{_,Value} -> 
	    try Validate(Value) of
		false -> throw({error, {badarg,[Key]}});
		true -> Value
	    catch
		error:_ ->
		    throw({error, {badarg,[Key]}})
	    end
    end.

is_string(Value) ->
    (erlang:iolist_size(Value) >= 0).
is_port_number(Value) ->
    is_integer(Value) andalso (Value > 0) andalso (Value < 65536).
is_boolean_or_string(Value) ->
    is_boolean(Value) orelse is_string(Value).

is_positive_number(V) -> is_integer(V) andalso (V > 0).
is_tls(V) -> lists:member(V, [if_available,always,never]).
is_auth(V) -> lists:member(V, [if_available,always,never]).


is_body(Value) ->
    case Value of
	{text,Body} -> 
	    (erlang:iolist_size(Body) >= 0);
	{image,_IType,_File} ->
	    %% fixme: check valid variants
	    true;
	Body when is_list(Body); is_binary(Body) ->
	    (erlang:iolist_size(Body) >= 0);
	_ ->
	    false
    end.

is_rfc822(Value) ->
    case smtp_util:parse_rfc822_addresses(Value) of
	{ok,[_]} -> true;
	_ -> false
    end.

is_rfc822_list(Value) ->
    case smtp_util:parse_rfc822_addresses(Value) of
	{ok,[_|_]} -> true;
	_ -> false
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
    
