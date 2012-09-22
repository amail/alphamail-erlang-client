Comfirm AlphaMail Erlang Client (v1.0.0)
======================

This module is the official client library for sending transactional emails with the cloud service AlphaMail.
To use this service you need an account. You can sign up for an free account on our website (http://www.comfirm.se). 

This is not a service for sending SPAM, newsletters or bulk emails of any kind. This is for transactional emails exclusive. 
Read more about transactional emails on http://www.comfirm.se.


## Example

    $ ./rebar compile
    $ erl -pa ebin/ /path/to/mochiweb/ebin/

    Service = alphamail:email_service("YOUR-ACCOUNT-API-TOKEN-HERE").
    Payload = alphamail:message_payload(
    	2,												% Project id
    	alphamail:email_contact(<<"Sender Name">>, <<"sender@domain.org">>),				% Sender
    	alphamail:email_contact(1234, <<"Joe E. Receiver">>, <<"email-of-receiver@comfirm.se">>),	% Receiver (with receiver id)
    	% JSON serializable payload data
    	[
    		{"id", 1234},
    		{"name", {struct, [
    			{"first", "Joe"},
    			{"last", "E. Receiver"},
    		]}},
    		{"dateOfBirth", 1989}
    	]
    ).
    alphamail:queue(Service, Payload).

## Dependencies

This module requires these other modules and libraries:

 * mochijson
