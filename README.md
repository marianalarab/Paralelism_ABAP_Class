# Paralelism_ABAP_Class
Paralelism with ABAP Class

The idea is that the main job, the father, will control its children jobs. When the child finishes their process than they will export the data to a cluster table. When all children finishes, the father then will import the data from the cluster table and clear it.

ZARCH_EXTRACT.abap -> Is a Report that is using the parallelism class to extract all SD data that can be archived. It is a huge logic that works with a lot of data.
ZCL_PARALLEL_JOB.abap -> Is the main class
ZCL_PARALLEL_JOB_DAO.abap -> Is needed so the test class can work without accessing de actual database and run its unit tests with coverage.
lcl_parallel_job_dao -> Is the local implementation of the DAO class that simulates de access to the database
lcl_parallel_job_test_class -> Is the Unit Test Class

<b>Other elements that needs to be created for this solution:</b></n>

ZPARALLEL_CONTROL -> table where the parameters of the parallelism such as how many job can be executed at the same time and the package size. For example.

@EndUserText.label : 'Parallelism Control'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zparallel_control {
  key mandt : mandt not null;
  key repid : repid not null;
  key vari  : variant not null;
  inexec    : zexec;
  msgty     : zmsgty;
  pacsize   : zpacsize;
  simul     : zsimul;
  uname     : uname;
  datum     : datum;
  uzeit     : uzeit;

}

ZTT_JOBS -> Table type of the structure below to use as parameters type.

@EndUserText.label : 'Jobs in process - Parallelism'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
define structure zlt_jobs {
  jobname  : btcjob;
  jobcount : btcjobcnt;
  status   : btcstatus;

}

And Of course the custer table -> This should be copied from the INDX standard table.
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
  loekz     : sychar01;
  sperr     : sychar01;
  aedat     : sydats;
  usera     : username;
  pgmid     : progname;
  begdt     : sydats;
  enddt     : sydats;
  clustr    : indx_clstr;
  clustd    : indx_clust;

}

