-module(example_simple).
-export([sendmail/0]).


%% Simple AlphaMail example
sendmail() ->
	Service = alphamail:email_service("50315633625b12-91553543"),
	Payload = alphamail:message_payload(
		143,											% Project id
		alphamail:email_contact(<<"Timothy">>, <<"timothy.johansson@comfirm.se">>),		% Sender
		alphamail:email_contact(1234, <<"Timothy">>, <<"timothy.johansson@comfirm.se">>),	% Receiver
		% JSON serializable payoad data
		[
			{"message", <<"Hello world like a boss!">>}, 							% Represents the <# payload.message #> in our template
			{"some_other_message", <<"And to the rest of the world! Chíkmàa! مرحبا! नमस्ते! Dumelang!">>}	% Represents the <# payload.some_other_message #> in our template
		]
	),
	alphamail:queue(Service, Payload).

