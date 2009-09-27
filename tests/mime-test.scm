
(use test hato-mime)

(test-begin)

(test '(mime (^ ("from" . "\"Dr. Watson <guest@grimpen.moor>\"")
                ("to" . "\"Sherlock Homes <not-really@221B-baker.street>\"")
                ("subject" . "\"First Report\"")
                ("content-type" . "text/plain; charset=\"ISO-8859-1\""))
             "Moor is gloomy. Heard strange noise, attached.\n")
    (call-with-input-string
     "From:    \"Dr. Watson <guest@grimpen.moor>\"
To:      \"Sherlock Homes <not-really@221B-baker.street>\"
Subject: \"First Report\"
Content-Type: text/plain; charset=\"ISO-8859-1\"

Moor is gloomy. Heard strange noise, attached.

"
     mime-message->sxml))

(test-end)
