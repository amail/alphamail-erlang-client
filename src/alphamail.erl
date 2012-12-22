%%
%% Erlang AlphaMail Client
%%
%% Copyright (c) 2012, Comfirm AB
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
%%     * Neither the name of the Comfirm AB nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(alphamail).
-author("timothy.johansson@comfirm.se").
-export([email_service/1, email_service/2, queue/2, message_payload/4, email_contact/2, email_contact/3]).
-define(SERVICE_URL, "http://api.amail.io/v2").


%% Email service
email_service(ApiToken) ->
	[{url, ?SERVICE_URL}, {token, ApiToken}].
	
email_service(ServiceUrl, ApiToken) ->
	[{url, ServiceUrl}, {token, ApiToken}].

%% Message payload
message_payload(ProjectId, Sender, Receiver, BodyObject) ->
	[{projectid, ProjectId}, {sender, Sender}, {receiver, Receiver}, {body, BodyObject}].

%% Email contact
email_contact(Name, Email) ->
	[{id, 0}, {name, Name}, {email, Email}].
	
email_contact(Name, Email, Id) ->
	[{id, Id}, {name, Name}, {email, Email}].

%% POST /email/queue
queue(Service, Payload) ->
	inets:start(),
    	
    	% Service and payload data
    	[{url, ServiceUrl}, {token, ApiToken}] = Service,
    	[{projectid, ProjectId}, {sender, Sender}, {receiver, Receiver}, {body, BodyObject}] = Payload,
    	
    	% Sender and receiver data
    	[{_,_}, {name, SenderName}, {email, SenderEmail}] = Sender,
    	[{id, ReceiverId}, {name, ReceiverName}, {email, ReceiverEmail}] = Receiver,
    	
    	% Serialize to JSON
    	Data = mochijson:encode({struct, [
    		{"project_id", ProjectId},
    		{"sender", {struct, [
    			{"name", SenderName},
    			{"email", SenderEmail}
    		]}},
    		{"receiver", {struct, [
    			{"id", ReceiverId},
    			{"name", ReceiverName},
    			{"email", ReceiverEmail}
    		]}},
    		{"payload", {struct, BodyObject}}
    	]}),
    	
    	% HTTP info
    	ContentType = "application/json",
    	ContentLength = integer_to_list(length(Data)),
    	
    	Headers = [auth_header("", ApiToken), {"Host", extract_host(ServiceUrl)}, {"Content-Length", ContentLength}, {"Connection", "Close"}],
    	Options = [{body_format, string}, {headers_as_is, true}],
    	
    	% Make the HTTP request and handle the response
    	handle_response(httpc:request(post, {ServiceUrl ++ "/email/queue", Headers, ContentType, Data}, [], Options)).

%% Create basic HTTP auth header
auth_header(User, Pass) ->
	Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
	{"Authorization","Basic " ++ Encoded}.

%% Extract host from url
extract_host(Url) ->
	StartPos = string:str(Url, "://"),
	if 
	StartPos > 1, length(Url) > (StartPos + 3) ->
		Host = string:substr(Url, StartPos + 3),
		EndPos = string:str(Host, "/"),
		if 
		EndPos > 1 ->
			string:substr(Host, 1, EndPos - 1);
		true ->
			Host
		end;
	true ->
		""
	end.	

%% Handle the response
handle_response(Response) ->
	case Response of
		{ok, {{_, _, _}, _, Body}} -> 
			Json = mochijson:decode(Body),
			case Json of
				{struct, [{_, ErrorCode}, {_, Message}, {_, Result}]} ->
					Res = {ErrorCode, Message, Result};
				_Else ->
					Res = {-4, "Unknown", ""}
			end;						
		{error, Reason} -> 
			Res = {-4, Reason, ""};
		_Else ->
			Res = {-4, "Unknown", ""}
	end,
	Res.

