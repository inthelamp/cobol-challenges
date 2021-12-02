const https = require('https');
const fs = require('fs');
const lineByLine = require('n-readlines');

// Checking if a file exists or not
const doFileExist = ( fileName ) => {
    try {
        fs.accessSync(fileName, fs.F_OK);
        return true;
    } catch (error) {
        return false;
    }
}

// Saving claim data
const save = (rawData, fileName) => {
    try {
        const parseData = JSON.parse(rawData);       

        // For column names
        const columnsParsed = parseData.meta.view.columns;

        var columns = columnsParsed.map( row => {
            return row['id'] != '-1' ? row['name'] : '';                   
        });
        const totalLength = columns.length;

        columns = columns.filter(item => item !== '');
        const startIndex = totalLength - columns.length;

        // For actual data
        const dataParsed = parseData.data;
        const data = dataParsed.map( row => {
            return row.map((column, index) => {
                if (index >= startIndex) {
                    return column;
                } else {
                    return '';
                }     
            }).filter(item => item !== '').join();
        });  

        // Sorting data records
        data.sort();

        // Filtering out duplucated records
        var dataUniq  = [...new Set(data)];

        // Combining with column names
        dataUniq.unshift(columns.join(','));

        // Saving to a file
        fs.writeFileSync(fileName, dataUniq.join('\r\n'));

    } catch (e) {
        console.error(e.message);
    }    
}

// Downloading claims
const GetUnemploymentClaims = async ( url, fileName ) => {
    return new Promise((resolve) => {
        https.get(url, (res) => {
            const { statusCode } = res;
            const contentType = res.headers['content-type'];
        
            let error;
            if (statusCode !== 200) {
                error = new Error('Request Failed.\n' +
                                `Status Code: ${statusCode}`);
            } else if (!/^application\/json/.test(contentType)) {
                error = new Error('Invalid claims-type.\n' +
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
                // Saving claim data
                resolve(save(rawData, fileName));
            });
        }).on('error', (e) => {
            console.error(`Got error: ${e.message}`);
        });
    })
}

// Creating a file containg Record ID and Date as a key
const CreateKeys = ( inputFile, keyFile ) => {

    let isExisted = doFileExist(keyFile);
    if (isExisted) {  
        const liner = new lineByLine(inputFile);
        const keyLiner = new lineByLine(keyFile);
         
        let line;
        let keyLine;
                
        // Skip a line having column names
        liner.next();

        let keyData = [];
        while (keyLine = keyLiner.next()) {
            keyLine = keyLine.toString().replace( /[\r\n]+/gm, "" ); 
            line = liner.next();
            line = line.toString();
            if (keyLine === line.substring(0,28)) {
                keyData.push(keyLine);
            } else if (keyLine > line.substring(0,28)) {
                keyData.push(line.substring(0,28));
                keyData.push(keyLine);          
            } else {
                keyData.push(keyLine);
                keyData.push(line.substring(0,28));                                          
            }
        }

        // Sorting records
        keyData.sort();

        // Removing duplucated records
        keyDataUniq = [...new Set(keyData)];

        const tempKeyFile = "tempKeyFile.txt";         
        fs.writeFileSync(tempKeyFile, keyDataUniq.join('\r\n'));

        // Replacing the key file with the temp key file
        fs.unlinkSync(keyFile);   
        fs.renameSync(tempKeyFile, keyFile, function(err) {
            if ( err ) {
                console.log('ERROR: ' + err);
            }
        });        
    } else {
        const liner = new lineByLine(inputFile);
         
        var fd = fs.openSync(keyFile, 'w');

        let line;
                
        // Skip a line having column names
        liner.next();

        while (line = liner.next()) {   
            line = line.toString();
            fs.writeSync(fd, line.substring(0,28) + '\r\n');            
        }

        // close the file
        fs.closeSync(fd);
    }
}

// Merging claim files using the key records
const MergeFiles = ( keyFile, inputFile, dataFile  ) => {

    let isExisted = doFileExist(dataFile);
    if (isExisted) {      
        const keyData = fs.readFileSync(keyFile,{encoding:'utf8', flag:'r'}).split('\r\n');
        const numberOfLines = keyData.length;

        const inputData = fs.readFileSync(inputFile,{encoding:'utf8', flag:'r'}).split('\r\n');       
        const numberOfColumns = inputData[0].split(",").length - 2;   // Computing number of columns excluding Record ID and Data columns 

        const dataLiner = new lineByLine(dataFile);                

        const tempFile = "temp.csv";
        var fd = fs.openSync(tempFile, 'w');

        let newLine;
        let dataLine;
        let isFound;
        let count = 0;

        while (dataLine = dataLiner.next()) {
            dataLine = dataLine.toString().replace( /[\r\n]+/gm, "" ); 

            isFound = false;
            for (const inputLine of inputData) {
                if (!inputLine.startsWith("Record")) {
                    if (keyData[count] === inputLine.substring(0,28)) {
                        newLine = dataLine + inputLine.substring(28);
                        isFound = true;
                        break;
                    }           
                }
            } 

            if (!isFound) {
                newLine = dataLine;
                for (let i=0; i < numberOfColumns; ++i ) {
                    newLine = newLine + ',0';
                }      
            }

            count++;             
            if (count < numberOfLines) {
                fs.writeSync(fd, newLine + '\r\n');     
            } else {
                fs.writeSync(fd, newLine);     
            }
        }

        // close the file
        fs.closeSync(fd);

        fs.unlinkSync(dataFile);     
        fs.renameSync(tempFile, dataFile, function(err) {
            if ( err ) {
                console.log('ERROR: ' + err);
            }
        });
    } else {
        const keyData = fs.readFileSync(keyFile,{encoding:'utf8', flag:'r'}).split('\r\n');
        const inputData = fs.readFileSync(inputFile,{encoding:'utf8', flag:'r'}).split('\r\n');       
                
        // Computing number of columns
        const numberOfColumns = inputData[0].split(",").length;

        let newLine;
        let isFound;

        let data = keyData.map(keyLine => {     
            isFound = false;
            for (const inputLine of inputData) {
                if (!inputLine.startsWith("Record")) {
                    if (keyLine === inputLine.substring(0,28)) {
                        newLine = keyLine.replace(",","") + inputLine.substring(28);
                        isFound = true;
                        break;
                    }           
                }
            } 
            if (!isFound) {
                newLine = keyLine.replace(",","");
                for (let i=0; i < numberOfColumns - 2; ++i ) {
                    newLine = newLine + ',0';
                }      
            }
            return newLine;
        });

        fs.writeFileSync(dataFile, data.join('\r\n'));
    }
}

module.exports = {
    GetUnemploymentClaims,
    MergeFiles,
    CreateKeys
}