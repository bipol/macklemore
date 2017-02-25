const express = require('express');
const cors = require('cors');
const app = express();
app.use(cors());

app.get('/api/itinerary', (req, res) => {
    data = {
        "itinerary" :
        [
            { "title" : "Test",
              "eventTime" : "2017-02-25 00:45:07 UTC",
              "location" : "Macklemore's Home"
            },
            { "title" : "Test",
              "eventTime" : "2017-02-25 00:45:07 UTC",
              "location" : "Macklemore's Home"
            },
            { "title" : "Test",
              "eventTime" : "2017-02-25 00:45:07 UTC",
              "location" : "Macklemore's Home"
            },
            { "title" : "Test",
              "eventTime" : "2017-02-25 00:45:07 UTC",
              "location" : "Macklemore's Home"
            },
            { "title" : "Test",
              "eventTime" : "2017-02-25 00:45:07 UTC",
              "location" : "Macklemore's Home"
            },
            { "title" : "Test",
              "eventTime" : "2017-02-25 00:45:07 UTC",
              "location" : "Macklemore's Home"
            }
        ]
    }
    res.json(data);
});

app.listen(3002, function() {
    console.log("listening on 3001")
});
