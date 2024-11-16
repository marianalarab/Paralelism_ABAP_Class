CLASS ZCL_PARALLEL_JOB_DAO DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS ZCL_PARALLEL.

  PUBLIC SECTION.
protected section.

  methods OPEN_JOB
    importing
      !I_JOBNAME type BTCJOB
    exporting
      !E_JOBCOUNT type BTCJOBCNT .
  methods CLOSE_JOB
    importing
      !I_JOBNAME type BTCJOB
      !I_JOBCOUNT type BTCJOBCNT
      !I_STRTIMMED type BTCCHAR1
    returning
      value(RETURN) type SUBRC .
  methods STATUS_GET
    importing
      !I_JOBCOUNT type BTCJOBCNT
      !I_JOBNAME type BTCJOB
    returning
      value(STATUS) type BTCSTATUS
    raising
      CX_BAPI_EX .
  methods JOB_DELETE
    importing
      !I_JOBCOUNT type BTCJOBCNT
      !I_JOBNAME type BTCJOB
    raising
      CX_BAPI_EX .
  methods GET_CUSTOMIZING
    importing
      !I_REPID type REPID
      !I_VARIANT type VARIANT
    returning
      value(E_CONTROL) type ZPARALLEL_CONTROL
    raising
      CX_BAPI_EX .
  methods EXPORT_DATA
    importing
      !I_NAME type BTCJOB
      !T_TABLE type TABLE .
  methods IMPORT_DATA
    importing
      !I_NAME type BTCJOB
    exporting
      !T_TABLE type TABLE .
  methods SUBMIT_REPORT
    importing
      !I_REPORT type PROGRAMM
      !IT_SEL_SUB type RSPARAMS_TT
      !VL_DEBUG type C
      !JOBNAME type BTCJOB
      !JOBCOUNT type BTCJOBCNT .
  methods CHECK_FEATURE
    importing
      !I_FEATURE type ZEFEATURE
      !I_FUNCT type ZEFUNCT
    returning
      value(R_ACTIVE) type ZACTIVE .
private section.
ENDCLASS.



CLASS ZCL_PARALLEL_JOB_DAO IMPLEMENTATION.


  METHOD close_job.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = i_jobcount
        jobname              = i_jobname
        strtimmed            = i_strtimmed
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

    return = sy-subrc.


  ENDMETHOD.


  METHOD export_data.

    EXPORT tab_arq = t_table TO DATABASE ZPARALLEL_CLUSTER(st) ID i_name(22).

  ENDMETHOD.


  METHOD get_customizing.

    SELECT SINGLE *
    FROM ZPARALLEL_CONTROL
    INTO e_control
    WHERE repid EQ i_repid AND
           vari EQ i_variant.
    IF sy-subrc NE 0.

      "No customizing in ZPARALLEL_CONTROL found for program &1 and variant &2
      RAISE EXCEPTION TYPE cx_bapi_ex
        EXPORTING
          textid = VALUE scx_t100key( msgid = 'ZPARALLEL'
                                      msgno = '007'
                                      attr1 = i_repid
                                      attr2 = i_variant ).

    ENDIF.

  ENDMETHOD.


  METHOD import_data.

    IMPORT tab_arq = t_table
    FROM DATABASE ZPARALLEL_CLUSTER(st)
    ID i_name(22).

    DELETE FROM DATABASE ZPARALLEL_CLUSTER(st) ID i_name(22).

  ENDMETHOD.


  METHOD job_delete.

    CALL FUNCTION 'BP_JOB_DELETE'
      EXPORTING
        jobcount                 = i_jobcount   " ID number of the job
        jobname                  = i_jobname    " Job Name
        forcedmode               = space        " Continue deletion in spite of certain errors
        commitmode               = abap_true
      EXCEPTIONS
        cant_delete_event_entry  = 1
        cant_delete_job          = 2
        cant_delete_joblog       = 3
        cant_delete_steps        = 4
        cant_delete_time_entry   = 5
        cant_derelease_successor = 6
        cant_enq_predecessor     = 7
        cant_enq_successor       = 8
        cant_enq_tbtco_entry     = 9
        cant_update_predecessor  = 10
        cant_update_successor    = 11
        commit_failed            = 12
        jobcount_missing         = 13
        jobname_missing          = 14
        job_does_not_exist       = 15
        job_is_already_running   = 16
        no_delete_authority      = 17
        OTHERS                   = 18.
    IF sy-subrc <> 0.
      "Error on deleting Job
      RAISE EXCEPTION TYPE cx_bapi_ex
        EXPORTING
          textid = VALUE scx_t100key( msgid = 'ZPARALLEL'
                                      msgno = '006' ).
    ENDIF.

  ENDMETHOD.


  METHOD open_job.

* Cria o job.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname  = i_jobname
      IMPORTING
        jobcount = e_jobcount.

  ENDMETHOD.


  METHOD status_get.
    CALL FUNCTION 'BP_JOB_STATUS_GET'
      EXPORTING
        jobcount                   = i_jobcount  " Job ID
        jobname                    = i_jobname  " Background job name
      IMPORTING
        status                     = status   " State of Background Job
      EXCEPTIONS
        job_doesnt_exist           = 1
        unknown_error              = 2
        parent_child_inconsistency = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      "Could not get status
      RAISE EXCEPTION TYPE cx_bapi_ex
        EXPORTING
          textid = VALUE scx_t100key( msgid = 'ZPARALLEL'
                                      msgno = '005' ).
    ENDIF.
  ENDMETHOD.


  METHOD submit_report.

    IF vl_debug IS INITIAL.
      SUBMIT (i_report)
      WITH SELECTION-TABLE it_sel_sub
        AND RETURN
        VIA JOB jobname
        NUMBER jobcount.
    ELSE.
      SUBMIT (i_report)
          WITH SELECTION-TABLE it_sel_sub
          AND RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
