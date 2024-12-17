# Paralelism with ABAP Class
This program was developed in Nov/2022
SAP version: ECC 6.0
HANA Database

The idea is that the main job, the father, will control its children jobs. When the child finishes their process than they will export the data to a cluster table. When all children finishes, the father then will import the data from the cluster table and clear it.

<li>ZARCH_EXTRACT.abap -&gt; Is a Report that is using the parallelism class to extract all SD data that can be archived. It is a huge logic that works with a lot of data.</li>
<li>ZCL_PARALLEL_JOB.abap -&gt; Is the main class</li>
<li>ZCL_PARALLEL_JOB_DAO.abap -&gt; Is needed so the test class can work without accessing de actual database and run its unit tests with coverage.</li>
<li>lcl_parallel_job_dao -&gt; Is the local implementation of the DAO class that simulates de access to the database</li>
<li>lcl_parallel_job_test_class -&gt; Is the Unit Test Class</li>
</br>
Other elements that needs to be created for this solution:
</br>

<li>ZPARALLEL_CONTROL -&gt; table where the parameters of the parallelism such as how many job can be executed at the same time and the package size. For example.</li>

```cds
@EndUserText.label : 'Parallelism Control'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED

define table zparallel_control {
  key mandt : mandt not null;
  key repid : repid not null;
  key vari : variant not null;
  inexec : zexec;
  msgty : zmsgty;
  pacsize : zpacsize;
  simul : zsimul;
  uname : uname;
  datum : datum;
  uzeit : uzeit;
```


<li>ZTT_JOBS -&gt; Table type of the structure below to use as parameters type.</li>

```cds
@EndUserText.label : 'Jobs in process - Parallelism'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE

define structure zlt_jobs {
  jobname : btcjob;
  jobcount : btcjobcnt;
  status : btcstatus;
```



<li>zparallel_cluster -&gt; And Of course the custer table -&gt; This should be copied from the INDX standard table.</li>

```cds
@EndUserText.label : 'Systemtabelle INDX'
@AbapCatalog.enhancement.category : #NOT_CLASSIFIED
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.activationType : #ADAPT_C_STRUCTURES
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED

define table zparallel_cluster {
  key mandt : mandt not null;
  key relid : indx_relid not null;
  key srtfd : indx_srtfd not null;
  key srtf2 : indx_srtf2 not null;
  loekz : sychar01;
  sperr : sychar01;
  aedat : sydats;
  usera : username;
  pgmid : progname;
  begdt : sydats;
  enddt : sydats;
  clustr : indx_clstr;
  clustd : indx_clust;
```
