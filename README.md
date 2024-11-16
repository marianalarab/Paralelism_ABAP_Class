<h3># Paralelism_ABAP_Class<br />Paralelism with ABAP Class</h3>
<p>The idea is that the main job, the father, will control its children jobs. When the child finishes their process than they will export the data to a cluster table. When all children finishes, the father then will import the data from the cluster table and clear it.</p>
<ul>
<li>ZARCH_EXTRACT.abap -&gt; Is a Report that is using the parallelism class to extract all SD data that can be archived. It is a huge logic that works with a lot of data.</li>
<li>ZCL_PARALLEL_JOB.abap -&gt; Is the main class</li>
<li>ZCL_PARALLEL_JOB_DAO.abap -&gt; Is needed so the test class can work without accessing de actual database and run its unit tests with coverage.</li>
<li>lcl_parallel_job_dao -&gt; Is the local implementation of the DAO class that simulates de access to the database</li>
<li>lcl_parallel_job_test_class -&gt; Is the Unit Test Class</li>
</ul>
<p><strong>Other elements that needs to be created for this solution:</strong></p>
<ul>
<li>ZPARALLEL_CONTROL -&gt; table where the parameters of the parallelism such as how many job can be executed at the same time and the package size. For example.</li>
</ul>
<p style="padding-left: 40px;">@EndUserText.label : 'Parallelism Control'<br />@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE<br />@AbapCatalog.tableCategory : #TRANSPARENT<br />@AbapCatalog.deliveryClass : #A<br />@AbapCatalog.dataMaintenance : #ALLOWED<br />define table zparallel_control {<br />key mandt : mandt not null;<br />key repid : repid not null;<br />key vari : variant not null;<br />inexec : zexec;<br />msgty : zmsgty;<br />pacsize : zpacsize;<br />simul : zsimul;<br />uname : uname;<br />datum : datum;<br />uzeit : uzeit;</p>
<p style="padding-left: 40px;">}</p>
<ul>
<li>ZTT_JOBS -&gt; Table type of the structure below to use as parameters type.</li>
</ul>
<p style="padding-left: 40px;">@EndUserText.label : 'Jobs in process - Parallelism'<br />@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE<br />define structure zlt_jobs {<br />jobname : btcjob;<br />jobcount : btcjobcnt;<br />status : btcstatus;</p>
<p style="padding-left: 40px;">}</p>
<ul>
<li>zparallel_cluster -&gt; And Of course the custer table -&gt; This should be copied from the INDX standard table.</li>
</ul>
<p style="padding-left: 40px;">@EndUserText.label : 'Systemtabelle INDX'<br />@AbapCatalog.enhancement.category : #NOT_CLASSIFIED<br />@AbapCatalog.tableCategory : #TRANSPARENT<br />@AbapCatalog.activationType : #ADAPT_C_STRUCTURES<br />@AbapCatalog.deliveryClass : #A<br />@AbapCatalog.dataMaintenance : #RESTRICTED<br />define table zparallel_cluster {<br />key mandt : mandt not null;<br />key relid : indx_relid not null;<br />key srtfd : indx_srtfd not null;<br />key srtf2 : indx_srtf2 not null;<br />loekz : sychar01;<br />sperr : sychar01;<br />aedat : sydats;<br />usera : username;<br />pgmid : progname;<br />begdt : sydats;<br />enddt : sydats;<br />clustr : indx_clstr;<br />clustd : indx_clust;</p>
<p style="padding-left: 40px;">}</p>
