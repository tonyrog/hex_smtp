HEX plugin for Email
====================

This plugin adds email sending capablities to Hex. The
Plugin using the excellent application gen_smtp to do this.
Look in rebar.config for details about how to find gen_smtp.


# Output event

    [{account,foo},
     {to, "Joe Smith<joe@mail.com>"},
     {subject, "Text"},
     {body, {text, "Testing123"}}]

    [{account,foo},
     {to, "Joe Smith<joe@mail.com>"},
     {subject, "Image"},
     {body, {image, <<"png">>, "/Picture/test/motorbike.png"}}
    ]

    [{account,foo},
     {to,"Joe Smith<joe@mail.com>,Bob Dog<bob@mail.com>"}
     {subject, "JPEG image test"},
     {body, {image, "jpeg", "/Picture/test/rose.jpg"}}
    ].


    [{account,x},
     {from,"rfc882-address"},
     {to,"rfc882-address(,rfc882-address)*"},
     {body,{text,iolist()} | {image,Type,<filename>} | iolist()},
     {subject,iolist()},
     [ {date,"rfc5322-timestamp"} ]
     [ {message_id,"id"} ]
