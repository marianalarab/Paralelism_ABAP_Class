CLASS ZCL_PARALLEL_JOB DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    METHODS start_parallelism
      IMPORTING
        !i_strtimmed TYPE btcchar1 DEFAULT 'X'
        !it_sel      TYPE rsparams_tt
        !i_report    TYPE programm
        !i_variant   TYPE variant
      RAISING
        cx_bapi_ex.

    METHODS get_customizing
      IMPORTING
        !i_repid         TYPE repid
        !i_variant       TYPE variant
      RETURNING
        VALUE(e_control) TYPE ZPARALLEL_CONTROL
      RAISING
        cx_bapi_ex.

    METHODS export_data
      IMPORTING
        !i_name  TYPE btcjob
        !t_table TYPE table.

    METHODS import_data
      IMPORTING
        i_name  TYPE btcjob
      EXPORTING
        t_table TYPE table.

    METHODS get_all_jobs
      RETURNING VALUE(e_jobs_all) TYPE ZTT_JOBS

  PROTECTED SECTION.

PRIVATE SECTION.

  DATA it_jobs TYPE ZTT_JOBS .
  DATA it_jobs_all TYPE ZTT_JOBS .
  DATA o_dao TYPE REF TO ZCL_PARALLEL_JOB_DAO.
  DATA:
    r_st_done TYPE RANGE OF btcstatus .
  DATA v_count TYPE i .
  CONSTANTS: gc_fixed  TYPE c VALUE 'F', " parameter/select options should be passed to all parallel jobs
             gc_selopt TYPE c VALUE 'S', " is a select options
             gc_param  TYPE c VALUE 'P'. " is a parameter

  METHODS check_jobs_inexecution .
  METHODS get_available_jobs
    IMPORTING
      !i_repid     TYPE repid
      !i_variant   TYPE variant
    RETURNING
      VALUE(count) TYPE i
    RAISING
      cx_bapi_ex .
  METHODS start_job
    IMPORTING
      !i_strtimmed  TYPE btcchar1 DEFAULT 'X'
      !it_sel       TYPE rsparams_tt
      !i_report     TYPE programm
      !i_package_no TYPE i .
ENDCLASS.



CLASS ZCL_PARALLEL_JOB IMPLEMENTATION.


  METHOD check_jobs_inexecution.

    LOOP AT it_jobs ASSIGNING FIELD-SYMBOL(<fl_jobs>).

      TRY.
          "Get status
          <fl_jobs>-status = o_dao->status_get(
            EXPORTING
              i_jobcount = <fl_jobs>-jobcount
              i_jobname  = <fl_jobs>-jobname

          ).
        CATCH cx_bapi_ex.    "
          "Could not get status

      ENDTRY.

      "Check if the status is completed
      IF <fl_jobs>-status IN r_st_done.

        "job done
        TRY.
            o_dao->job_delete(
              EXPORTING
                i_jobcount = <fl_jobs>-jobcount
                i_jobname  = <fl_jobs>-jobname
            ).
          CATCH cx_bapi_ex.    "
        ENDTRY.

      ENDIF.

    ENDLOOP.

    DELETE it_jobs WHERE status IN r_st_done.

  ENDMETHOD.


  METHOD constructor.

    o_dao = NEW ZCL_PARALLEL_JOB_dao(  ).

    CLEAR v_count.

    r_st_done = VALUE #( ( low = 'A' sign = 'I' option = 'EQ' )
                         ( low = 'F' sign = 'I' option = 'EQ' )
                         ( low = 'Z' sign = 'I' option = 'EQ' )
                         ( low = 'X' sign = 'I' option = 'EQ' ) ).

  ENDMETHOD.


  METHOD export_data.
    o_dao->export_data(
      EXPORTING
        i_name  = i_name
        t_table = t_table
    ).
  ENDMETHOD.


  METHOD get_all_jobs.

    e_jobs_all = it_jobs_all.

  ENDMETHOD.


METHOD get_available_jobs.

    TRY.
        DATA(control) = o_dao->get_customizing(
                     i_repid    = i_repid
                     i_variant  = i_variant ).

      CATCH cx_bapi_ex INTO DATA(exception).
        RAISE EXCEPTION exception.
    ENDTRY.

    DATA(lines) = lines( it_jobs ).                                                              
    count = control-simul - lines.

  ENDMETHOD.


  METHOD get_customizing.
    TRY.
        e_control = o_dao->get_customizing(
                       i_repid    = i_repid
                       i_variant  = i_variant ).
      CATCH cx_bapi_ex INTO DATA(exception).
        RAISE EXCEPTION exception.
    ENDTRY.
  ENDMETHOD.


  METHOD import_data.

    o_dao->import_data(
      EXPORTING
        i_name    = i_name
      IMPORTING
        t_table = t_table
    ).

  ENDMETHOD.


METHOD start_job.

  DATA: jobname    TYPE btcjob,
        it_sel_sub TYPE TABLE OF rsparams,
        vl_debug   TYPE c.

  jobname = i_report && '_' && i_package_no.

  it_sel_sub[] = it_sel[].

  o_dao->open_job( EXPORTING i_jobname  = jobname
                   IMPORTING e_jobcount = DATA(jobcount) ).

  APPEND VALUE #( selname = 'P_JOBSUB'
                   kind    = 'P'
                   sign    = 'I'
                   option  = 'EQ'
                   low     = abap_true
                   high    = space ) TO it_sel_sub.

  APPEND VALUE #( selname = 'P_JOBNAM'
                   kind    = 'P'
                   sign    = 'I'
                   option  = 'EQ'
                   low     = jobname
                   high    = space ) TO it_sel_sub.


    o_dao->submit_report(
      EXPORTING
        i_report   = i_report
        it_sel_sub = it_sel_sub
        vl_debug   = vl_debug
        jobname    = jobname
        jobcount   = jobcount
    ).



  DATA(r_return) = o_dao->close_job( EXPORTING i_jobname   = jobname
                                               i_jobcount  = jobcount
                                               i_strtimmed = i_strtimmed   ).

  APPEND VALUE #(  jobname = jobname
                  jobcount = jobcount ) TO it_jobs.

  APPEND VALUE #(  jobname = jobname
                   jobcount = jobcount ) TO it_jobs_all.
ENDMETHOD.


METHOD start_parallelism.

    DATA:      it_sel_sub TYPE rsparams_tt,
               it_sel_aux TYPE rsparams_tt,
               it_sel_fix TYPE rsparams_tt,
               wa_sel_fix TYPE rsparams,
               lv_count   TYPE i,
               lv_pack_no TYPE i.
    TRY.
        DATA(control) = o_dao->get_customizing(
          EXPORTING
            i_repid   = i_report
            i_variant = i_variant
        ).

      CATCH cx_bapi_ex INTO DATA(exception).
        RAISE EXCEPTION exception.

    ENDTRY.

* get all fixed values (parameters are always fixed) and put them on a separate table
    CLEAR wa_sel_fix.
    it_sel_fix = it_sel.
    DELETE it_sel_fix WHERE kind <> gc_fixed AND kind <> gc_param.

    wa_sel_fix-kind = gc_selopt.
    MODIFY it_sel_fix FROM wa_sel_fix TRANSPORTING kind WHERE kind = gc_fixed.

    it_sel_aux = it_sel.
    DELETE it_sel_aux WHERE kind = gc_fixed OR kind = gc_param.

    DO.

      DATA(lines) = lines( it_jobs ).                                                            

      LOOP AT it_sel_aux ASSIGNING FIELD-SYMBOL(<fl_sel>).

        ADD 1 TO lv_count.
        APPEND <fl_sel> TO it_sel_sub.
        <fl_sel>-sign = 'E'.

        "IF the selection reaches the maximum size of the package
        IF lv_count EQ control-pacsize AND lines LT control-simul.
          ADD 1 TO lv_pack_no.

          APPEND LINES OF it_sel_fix TO it_sel_sub.

          me->start_job(
            EXPORTING
              it_sel       = it_sel_sub
              i_report     = i_report
              i_package_no = lv_pack_no ).

          CLEAR lv_count.

          me->check_jobs_inexecution( ).
          lines = lines( it_jobs ).                                                             
          EXIT.

        ELSEIF lines EQ control-simul.

          EXIT.

        ENDIF.

      ENDLOOP.

      "IF the last package does not have the maximum size
      IF lv_count NE 0 AND lv_count LE control-pacsize AND lines LT control-simul.
        ADD 1 TO lv_pack_no.

        APPEND LINES OF it_sel_fix TO it_sel_sub.

        me->start_job(
          EXPORTING
            it_sel       = it_sel_sub
            i_report     = i_report
            i_package_no = lv_pack_no ).

        CLEAR lv_count.

        me->check_jobs_inexecution( ).
        lines = lines( it_jobs ).                                                             

      ENDIF.

      "Remove the selection that is already in execution by the job started.
      DELETE it_sel_aux WHERE sign EQ 'E'.

      "IF there is max of jobs running simultaneously then wait until at least one finishes before starting a new one.
      WHILE lines EQ control-simul.

        WAIT UP TO 1 SECONDS.

        me->check_jobs_inexecution( ).
        lines = lines( it_jobs ).                                                             

      ENDWHILE.

      "Clear it_sel_sub for the next execution
      CLEAR it_sel_sub.

      IF lines EQ 0 AND it_sel_aux IS INITIAL.

        EXIT.

      ELSEIF it_sel_aux IS INITIAL.

        me->check_jobs_inexecution( ).
        lines = lines( it_jobs ).                                                               

      ENDIF.

    ENDDO.

  ENDMETHOD.
ENDCLASS.
