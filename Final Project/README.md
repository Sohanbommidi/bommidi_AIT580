# DATA-ANALYSIS-OF-COLLEGE-SCORECARD

## RAW DATA FILE
* As rawdata file size is above 100MB and we were unable to upload the rawdata file to github
* we can download rawdata file in two ways
### Method 1
* we can download the zip file of rawdata form [here](https://catalog.data.gov/dataset/college-scorecard)

### Method 2
It can fetched by API copy below code and run. This code will download the zip file and extract the all academic year data file.
```
folder_name = r'../data'


if not os.path.exists(folder_name):
    os.makedirs(folder_name)


url = r'https://ed-public-download.app.cloud.gov/downloads/CollegeScorecard_Raw_Data.zip'
response = requests.get(url, stream = True)


with zipfile.ZipFile(io.BytesIO(response.content)) as zf:
    for elem in zf.namelist():
        zf.extract(elem, '../data')
        print(elem)
```
* We are usinng the data file for Academic year 2018-2019
* Below code will load the csv file of Academic year 2018-2019.
```
dfRawData = pd.read_csv('../data/CollegeScorecard_Raw_Data/MERGED2018_19_PP.csv', low_memory = False)
```

## Steps to run the analysis scripts
### Clone repository
```
https://github.com/vnshanmukh/DATA-ANALYSIS-OF-COLLEGE-SCORECARD
```
* From analysis folder open ait.ipynb in jupyter notebook or google collabrator
