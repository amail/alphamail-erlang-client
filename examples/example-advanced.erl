-module(example_advanced).
-export([sendmail/0]).


%% AlphaMail example with error handling
sendmail() ->
	% Step 1: Let's start by entering the web service URL and the API-token you've been provided
	% If you haven't gotten your API-token yet. Log into AlphaMail or contact support at "support@comfirm.se".
	Service = email_service("http://api.amail.io/v2", "YOUR-ACCOUNT-API-TOKEN-HERE"),
	
	% Step 2: Let's fill in the gaps for the variables (stuff) we've used in our template
	Message = [
		{"message", <<"Hello world like a boss!">>}, 							% Represents the <# payload.message #> in our template
		{"some_other_message", <<"And to the rest of the world! Chíkmàa! مرحبا! नमस्ते! Dumelang!">>}	% Represents the <# payload.some_other_message #> in our template
	],
	
	% Step 3: Let's set up everything that is specific for delivering this email
	Payload = message_payload(
		2,												% Project Id
		email_contact(<<"Sender Company Name">>, <<"your-sender-email@your-sender-domain.com">>),	% Sender
		email_contact(0, <<"Joe E. Receiver">>, <<"email-of-receiver@comfirm.se">>),			% Receiver (with receiver id)
		Message												% JSON serializable payload data
	),
	
	% Step 4: Haven't we waited long enough. Let's send this!
	Response = queue(Service, Payload),
	case Response of
		{0, Message, Result} ->
			% OK
			% Step #5: Pop the champagné! We got here which mean that the request was sent successfully and the email is on it's way!        
			io:format("Successfully queued message with id '~s' (you can use this ID to get more details about the delivery)~n", [Result]);
		{-1, Message, _} ->
			% AUTHENTICATION ERROR
			% Ooops! You've probably just entered the wrong API-token.
			io:format("Authentication error: ~s (~p)~n", [Message, -1]);
		{-2, Message, _} ->
			% VALIDATION ERROR
			% Something in the input was wrong. Probably good to double double-check!
			io:format("Validation error: ~s (~p)~n", [Message, -2]);
		{-3, Message, _} ->
			% INTERNAL ERROR
			% Not that it is going to happen... Right :-)
			io:format("Internal error: ~s (~p)~n", [Message, -3]);
		{-4, Message, _} ->
			% UNKNOWN ERROR
			% Most likely your internet connection that is down. We are covered for most things except "multi-data-center-angry-server-bashing-monkeys" (remember who coined it) or.. nuclear bombs.
        		% If one blew. Well.. It's just likely that our servers are down.
			io:format("An error (probably related to connection) occurred: ~s~n", [Message]);
		_Else ->
			% UNKNOWN ERROR
			io:format("An unknown error occurred~n")
	end,
	
	% Writing to out like a boss
	io:format("~n~nIn doubt or experiencing problems?~n" ++
 		  "Please email our support at support@comfirm.se~n").
 