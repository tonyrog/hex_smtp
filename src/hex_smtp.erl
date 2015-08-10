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
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
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
    Flags = 
	case proplists:get_value(account, Flags0) of
	    {name,Name} ->
		{ok,Fs1} = application:get_env(hex_smtp, Name),
		proplists:delete(account,Flags0) ++ Fs1;
	    {config,Fs1} ->
		proplists:delete(account,Flags0) ++ Fs1
	end,
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
	    {image,IFlags} ->
		File  = proplists:get_value(filename, IFlags),
		MType = proplists:get_value(mime_type, IFlags),
		{ok,Bin} = file:read_file(File),
		case string:tokens(MType, "/") of
		    [SubType0] ->
			{<<"image">>,to_binary(SubType0),Bin};
		    [Type0,SubType0] ->
			{to_binary(Type0),to_binary(SubType0),Bin}
		end
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
%% mod_event(in | out, Flags::[{atom(),term()}])
%%
mod_event(_, _) ->
    ok.


%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(Dir, Flags) ->
    %% FIXME: handle account setting a bit special 
    %%        check that account exist and that it 
    %%        is checked against account config.
    %% FIXME: handle yang standard types
    %% FIXME: handle typedefs in a general way?
    hex:validate_flags(Flags,event_spec(Dir)).

event_spec(in) ->
    [];
event_spec(out) ->
    output_spec().

output_spec() ->
    [
     {choice,account,
      [{'case',name,[{leaf,name,[{type,string,[]}]}]},
       {'case',config,
	[{container,config,
	  [{leaf, relay, [{type, 'yang:domain-name', []},
			  {mandatory,true,[]}]},
	   {leaf, port, [{type, 'yang:port-number',[]}]},
	   {leaf, ssl, [{type, boolean, []},{default,false,[]}]},
	   {leaf, username, [{type, string,[]}]},
	   {leaf, password, [{type, string, []}]},
	   {leaf, tls, [{type,enumeration,
			 [{enum,if_available,[]},
			  {enum,always,[]},
			  {enum,never,[]}]},
			{default,if_available,[]}]},
	   
	   {leaf, auth, [{type,enumeration,
			  [{enum,if_available,[]},
			   {enum,always,[]},
			   {enum,never,[]}]},
			 {default,if_available,[]}]},
	   {leaf, retries, [{type, uint32, []},
			    {default, 1, []}]},
	   {leaf, date, [{type, string, []},
			 {default, false, []}]},
	   {leaf, message_id, [{type, string, []},
			       {default, false, []}]}
	  ]}
	]}
      ]},
     
     {leaf, from, [{type,'hex:rfc822',[]},
		   {mandatory, true, []}]},

     {'leaf-list', to, [{type,'hex:rfc822',[]},
		      {'min-elements',1,[]}]},

     {leaf, subject, [{type, string, []},
		      {default, "", []}]},

     %% FIXME: make mixed body data!
     {choice, body,
      [{'case',text,
	[{leaf, text, [{type,string,[]}]}]},
       {'case',image,
	[{container, image, 
	  [{leaf, filename, [{type, string, []},{mandatory,true,[]}]},
	   {leaf, mime_type, [{type, string, []},{default,"image/png",[]}]}
	  ]}]}]},

     {leaf, date, [{type, string, []},
		   {default, false, []}]},
     {leaf, message_id, [{type, string, []},
			 {default, false, []}]}
    ].

%% is_rfc822(Value) ->
%%    try smtp_util:parse_rfc822_addresses(Value) of
%%	{ok,[_]} -> true;
%%	_ -> false
%%    catch
%%	error:_ -> false
%%    end.

%% is_rfc822_list(Value) ->
%%    try smtp_util:parse_rfc822_addresses(Value) of
%%	{ok,[_|_]} -> true;
%%	_ -> false
%%    catch
%%	error:_ -> false
%%    end.


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

