const { GetUnemploymentClaims, MergeFiles, CreateKeys } = require('./controllers/UnemploymentClaims');  

const keyFile = "./data/recordIDDate.txt";
const dataFile = "./data/claims.csv";
const inputFileByAge = "./data/claimsByAge.csv";
const inputFileByEthnicity = "./data/claimsByEthnicity.csv";
const inputFileByIndustry = "./data/claimsByIndustry.csv";
const inputFileByRace = "./data/claimsByRace.csv";
const inputFileByGender = "./data/claimsByGender.csv";

const getClaims = async () => {
    let url = 'https://data.mo.gov/api/views/5tqh-2x4m/rows.json?accessType=DOWNLOAD';
    await GetUnemploymentClaims(url, inputFileByAge);

    url = 'https://data.mo.gov/api/views/xm42-6a8n/rows.json?accessType=DOWNLOAD';
    await GetUnemploymentClaims(url, inputFileByEthnicity);

    url = 'https://data.mo.gov/api/views/cj66-t7xq/rows.json?accessType=DOWNLOAD';
    await GetUnemploymentClaims(url, inputFileByIndustry);

    url = 'https://data.mo.gov/api/views/cq57-7qrb/rows.json?accessType=DOWNLOAD';
    await GetUnemploymentClaims(url, inputFileByRace);

    url = 'https://data.mo.gov/api/views/4v5t-4kqk/rows.json?accessType=DOWNLOAD';
    await GetUnemploymentClaims(url, inputFileByGender);
}

const CreateClaimData = () => {
    const getClaimsPromise = new Promise((resolve) => {
        resolve(getClaims());
    });

    getClaimsPromise.then(() => {
        CreateKeys(inputFileByAge, keyFile);
        CreateKeys(inputFileByEthnicity, keyFile);
        CreateKeys(inputFileByIndustry, keyFile);
        CreateKeys(inputFileByRace, keyFile);
        CreateKeys(inputFileByGender, keyFile);
        MergeFiles(keyFile,inputFileByAge,dataFile);
        MergeFiles(keyFile,inputFileByEthnicity,dataFile);
        MergeFiles(keyFile,inputFileByIndustry,dataFile);
        MergeFiles(keyFile,inputFileByRace,dataFile);
        MergeFiles(keyFile,inputFileByGender,dataFile);
    });
}

CreateClaimData();