*"* use this source file for your ABAP unit test classes
CLASS lcl_parallel_job_dao DEFINITION INHERITING FROM ZCL_PARALLEL_JOB_DAO FINAL.

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS open_job        REDEFINITION.
    METHODS close_job       REDEFINITION.
    METHODS status_get      REDEFINITION.
    METHODS job_delete      REDEFINITION.
    METHODS get_customizing REDEFINITION.
    METHODS export_data     REDEFINITION.
    METHODS import_data     REDEFINITION.
    METHODS submit_report   REDEFINITION.
    METHODS check_feature   REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_vbeln,
             vbeln TYPE vbeln,
           END OF ty_vbeln.

    DATA: gv_repid    TYPE repid,
          gv_variant  TYPE variant,
          ge_control  TYPE zparallel_control,
          gt_sel      TYPE rsparams_tt,
          gv_jobcount TYPE btcjobcnt,
          gv_return   TYPE subrc,
          gv_status   TYPE btcstatus,
          gt_table    TYPE TABLE OF ty_vbeln.

ENDCLASS.

CLASS lcl_parallel_job_dao IMPLEMENTATION.

  METHOD open_job.
    e_jobcount = '03182500'.
  ENDMETHOD.

  METHOD close_job.
    return = '00'.
  ENDMETHOD.

  METHOD status_get.

    IF i_jobname EQ 'REPORT_ERROR_4233'.
      RAISE EXCEPTION TYPE cx_bapi_ex.
    ELSE.
      status = 'A'.
    ENDIF.
  ENDMETHOD.

  METHOD job_delete.

    IF i_jobname EQ 'REPORT_ERROR_4234'.
      RAISE EXCEPTION TYPE cx_bapi_ex.
    endif.

  ENDMETHOD.

  METHOD get_customizing.

    IF i_repid EQ 'AAA'.
      RAISE EXCEPTION TYPE cx_bapi_ex.

    ELSEIF i_repid EQ 'ZVMA0_ARCH_EXTRACT'.
      e_control = VALUE #(  mandt    = '410'
                            repid    = 'ZVMA0_ARCH_EXTRACT'
                            vari     = 'G1_2004_2009'
                            pacsize  = '2'
                            simul    = '050'                ).

    ELSEIF i_repid EQ 'ZHMA0_ARCH_EXTRACT'.
      e_control = VALUE #(  mandt    = '410'
                           repid    = 'ZVMA0_ARCH_EXTRACT'
                           vari     = 'G1_2004_2009'
                           pacsize  = '2'
                           simul    = '070'                 ).

    ELSE.
      e_control = VALUE #(  mandt    = '410'
                            repid    = 'ZMA0_ARCH_EXTRACT'
                            vari     = 'G1_2004_2009'
                            pacsize  = '1'
                            simul    = '050'                ).

    ENDIF.

  ENDMETHOD.

  METHOD export_data.

  ENDMETHOD.

  METHOD import_data.
    t_table = gt_table.
  ENDMETHOD.

  METHOD submit_report.

  ENDMETHOD.

  METHOD check_feature.
    r_active = abap_true.
  ENDMETHOD.

ENDCLASS.
