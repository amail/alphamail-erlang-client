Comfirm AlphaMail Erlang Client
======================

Use this module to connect your Erlang applications to the [AlphaMail](http://www.comfirm.se) service. Start sending transactional email today without the hassle of SMTP and large blocks of unmaintainable HTML.

AlphaMail supports templating ([Comlang](http://amail.io/#/docs/comlang/)), DKIM signatures and reliable delivery. Beside that, we got awesome statistics.

To use this service you need an account. You can sign up for a free account on our website.

http://www.comfirm.se/

This is not a service for sending SPAM, newsletters or bulk emails of any kind. This is for transactional emails exclusive. 
Read more about transactional emails [here](http://comfirm.se/transactional-email/).


## Example

    $ ./rebar compile
    $ erl -pa ebin/ /path/to/mochiweb/ebin/

    Service = alphamail:email_service("YOUR-ACCOUNT-API-TOKEN-HERE").
    Payload = alphamail:message_payload(
    	2,												% Project id
    	alphamail:email_contact(<<"Sender Name">>, <<"sender@domain.org">>),				% Sender
    	alphamail:email_contact(<<"Joe E. Receiver">>, <<"email-of-receiver@comfirm.se">>, 1234),	% Receiver, the 3rd argument is the optional receiver id and should be either a string or an integer
    	% JSON serializable payload data
    	[
    		{"userId", 1234},
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
