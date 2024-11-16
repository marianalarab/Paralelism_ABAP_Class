# Paralelism_ABAP_Class
Paralelism with ABAP Class

The idea is that the main job, the father, will control its children jobs. When the child finishes their process than they will export the data to a cluster table. When all children finishes, the father then will import the data from the cluster table and clear it.

ZARCH_EXTRACT.abap -> Is a Report that is using the parallelism class to extract all SD data that can be archived. It is a huge logic that works with a lot of data.
ZCL_PARALLEL_JOB.abap -> Is the main class
ZCL_PARALLEL_JOB_DAO.abap -> Is needed so the test class can work without accessing de actual database and run its unit tests with coverage.
lcl_parallel_job_dao -> Is the local implementation of the DAO class that simulates de access to the database
lcl_parallel_job_test_class -> Is the Unit Test Class

<b>Other elements that needs to be created for this solution:</b>
ZPARALLEL_CONTROL -> table where the parameters of the parallelism such as how many job can be executed at the same time and the package size. For example.


