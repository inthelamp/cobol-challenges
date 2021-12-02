const https = require('https');
const fs = require('fs');

const GetSummary = () => {
  https.get('https://api.covid19api.com/summary', (res) => {
      const { statusCode } = res;
      const contentType = res.headers['content-type'];
    
      let error;
      if (statusCode !== 200) {
        error = new Error('Request Failed.\n' +
                          `Status Code: ${statusCode}`);
      } else if (!/^application\/json/.test(contentType)) {
        error = new Error('Invalid summary-type.\n' +
                          `Expected application/json but received ${contentType}`);
      }
      if (error) {
        console.error(error.message);
        // Consume response data to free up memory
        res.resume();
        return;
      }
    
      res.setEncoding('utf8');
      let rawData = '';
      res.on('data', (chunk) => { rawData += chunk; });
      res.on('end', () => {
        try {
          const parseData = JSON.parse(rawData);    
          const contries = parseData.Countries;

          var columns =  Object.keys(contries[0]);
          columns.splice(columns.length -1, 1);
          var summary = contries.map(row => {
              return columns.map( column => {
                                  return JSON.stringify(row[column]);
                                }).join(',');
          });        
          summary.unshift(columns.join(','));     
          const summaryCSV = summary.join('\r\n');    
          fs.writeFileSync("./data/summary.csv", summaryCSV);
        } catch (e) {
          console.error(e.message);
        }
      });
    }).on('error', (e) => {
        console.error(`Got error: ${e.message}`);
    });
}

module.exports = GetSummary;