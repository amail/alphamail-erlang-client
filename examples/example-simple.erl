-module(example_simple).
-export([sendmail/0]).


%% Simple AlphaMail example
sendmail() ->
	Service = alphamail:email_service("YOUR-ACCOUNT-API-TOKEN-HERE"),
	Payload = alphamail:message_payload(
		2,												% Project id
		alphamail:email_contact(<<"Sender Name">>, <<"sender@domain.org">>),				% Sender
		alphamail:email_contact(0, <<"Joe E. Receiver">>, <<"email-of-receiver@comfirm.se">>),		% Receiver (with receiver id)
		% JSON serializable payload data
		[
			{"message", <<"Hello world like a boss!">>}, 							% Represents the <# payload.message #> in our template
			{"some_other_message", <<"And to the rest of the world! Chíkmàa! مرحبا! नमस्ते! Dumelang!">>}	% Represents the <# payload.some_other_message #> in our template
		]
	),
	alphamail:queue(Service, Payload).
