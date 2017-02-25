const express = require('express');
const cors = require('cors');
const app = express();
app.use(cors());

app.get('/api/itinerary', (req, res) => {
    data = 
        [
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},
			{
			   "title": "string",
				"description": "string",
				"distance": 0,
				"event_time": 0,
				"location": {
                  "address" : "something",
				  "lat": 0,
				  "long": 0
				}
			},

		]
    res.json(data);
});

app.listen(3002, function() {
    console.log("listening on 3001")
});
