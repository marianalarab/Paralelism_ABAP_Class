CLASS lcl_parallel_job_test_class DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO ZPARALLEL_CONTROL,  "class under test
      f_dao TYPE REF TO lcl_parallel_job_dao.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: check_jobs_inexecution FOR TESTING.
    METHODS: check_jobs_inexecution_error FOR TESTING.
    METHODS: check_jobs_inexecution_error2 FOR TESTING.
    METHODS: export_data FOR TESTING.
    METHODS: get_all_jobs FOR TESTING.
    METHODS: get_available_jobs FOR TESTING.
    METHODS: get_customizing FOR TESTING.
    METHODS: get_customizing_fail FOR TESTING.
    METHODS: import_data FOR TESTING.
    METHODS: start_job FOR TESTING.
    METHODS: start_parallelism FOR TESTING.
    METHODS: start_parallelism_fail FOR TESTING.
    METHODS: start_parall_max_size FOR TESTING.
    METHODS: start_parall_not_max_size FOR TESTING.

ENDCLASS.      


CLASS lcl_parallel_job_test_class IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.


    CREATE OBJECT f_cut.
    CREATE OBJECT f_dao.
    f_cut->o_dao = f_dao.
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD check_jobs_inexecution.

    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_package_no TYPE i.

    i_strtimmed = 'X'.
    i_report = '/BDL/TASK_PROCESSOR'.
    i_package_no = '0000004233'.

    f_cut->start_job(
       i_strtimmed = i_strtimmed
        it_sel = it_sel
        i_report = i_report
        i_package_no = i_package_no ).

    f_cut->check_jobs_inexecution(  ).


  ENDMETHOD.

  METHOD check_jobs_inexecution_error.

    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_package_no TYPE i.

    i_strtimmed = 'X'.
    i_report = 'REPORT_ERROR'.
    i_package_no = '0000004233'.

    f_cut->start_job(
       i_strtimmed = i_strtimmed
        it_sel = it_sel
        i_report = i_report
        i_package_no = i_package_no ).

    f_cut->check_jobs_inexecution(  ).


  ENDMETHOD.

  METHOD check_jobs_inexecution_error2.

    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_package_no TYPE i.

    i_strtimmed = 'X'.
    i_report = 'REPORT_ERROR'.
    i_package_no = '0000004234'.

    f_cut->start_job(
       i_strtimmed = i_strtimmed
        it_sel = it_sel
        i_report = i_report
        i_package_no = i_package_no ).

    f_cut->check_jobs_inexecution(  ).


  ENDMETHOD.

  METHOD export_data.

    DATA i_name TYPE btcjob.
    DATA t_table TYPE TABLE OF string.

    i_name = '/BDL/TASK_PROCESSOR0000004233'.

    f_cut->export_data(
        i_name = i_name
        t_table = t_table ).

  ENDMETHOD.


  METHOD get_all_jobs.

    DATA e_jobs_all TYPE ZTT_JOBS.

    e_jobs_all = f_cut->get_all_jobs(  ).

  ENDMETHOD.


  METHOD get_available_jobs.

    DATA i_repid TYPE repid.
    DATA i_variant TYPE variant.
    DATA count TYPE i.

    count = f_cut->get_available_jobs(
        i_repid = i_repid
        i_variant = i_variant ).

    cl_abap_unit_assert=>assert_equals(
      act   = count
      exp   = '50'          "<--- please adapt expected value
      msg   = 'Testing value count'
    ).
  ENDMETHOD.

  METHOD get_customizing.

    DATA i_repid TYPE repid.
    DATA i_variant TYPE variant.
    DATA e_control TYPE /deere/parallel.

    i_repid = 'ZMA0_ARCH_EXTRACT'.
    i_variant = 'G1_2004_2009'.

    TRY.

        e_control = f_cut->get_customizing(
            i_repid = i_repid
            i_variant = i_variant ).

      CATCH cx_bapi_ex INTO DATA(error).
        error->get_text( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act   = e_control-repid
      exp   = 'ZMA0_ARCH_EXTRACT'         "<--- please adapt expected value
      msg   = 'Testing Exception'

    ).

  ENDMETHOD.


  METHOD get_customizing_fail.

    DATA i_repid TYPE repid.
    DATA i_variant TYPE variant.
    DATA e_control TYPE ZPARALELL_CONTROL.

    i_repid = 'AAA'.
    i_variant = 'G1_2004_2009'.

    TRY.

        e_control = f_cut->get_customizing(
            i_repid = i_repid
            i_variant = i_variant ).

      CATCH cx_bapi_ex INTO DATA(error).
        error->get_text( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act   = e_control-mandt
      exp   = ''          "<--- please adapt expected value
      msg   = 'Testing Exception'
    ).
  ENDMETHOD.


  METHOD import_data.

    DATA i_name TYPE btcjob.
    DATA t_table TYPE string.

    i_name = '/BDL/TASK_PROCESSOR0000004233'.

    f_cut->import_data(
      EXPORTING
        i_name = i_name
*     IMPORTING
*       T_TABLE = t_Table
    ).

  ENDMETHOD.


  METHOD start_job.

    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_package_no TYPE i.

    i_strtimmed = 'X'.
    i_report = '/BDL/TASK_PROCESSOR'.
    i_package_no = '0000004233'.

    f_cut->start_job(
       i_strtimmed = i_strtimmed
        it_sel = it_sel
        i_report = i_report
        i_package_no = i_package_no ).


  ENDMETHOD.


  METHOD start_parallelism.

    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_variant TYPE variant.

    it_sel = VALUE #( ( selname = 'P_JOBSUB'
                        kind    = 'P'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space )
                      ( selname = 'P_JOBNAM'
                        kind    = 'A'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space ) ).

    i_strtimmed = 'X'.
    i_report = 'ZMA0_ARCH_EXTRACT'.
    i_variant = 'G1_2004_2009'.


    f_cut->start_parallelism(
*       I_STRTIMMED = i_Strtimmed
        it_sel = it_sel
        i_report = i_report
        i_variant = i_variant ).

  ENDMETHOD.

  METHOD start_parallelism_fail.

    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_variant TYPE variant.

    it_sel = VALUE #( ( selname = 'P_JOBSUB'
                        kind    = 'A'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space )
                      ( selname = 'P_JOBNAM'
                        kind    = 'A'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space ) ).

    i_strtimmed = 'X'.
    i_report = 'AAA'.
    i_variant = 'G1_2004_2009'.

    TRY.

        f_cut->start_parallelism(
*       I_STRTIMMED = i_Strtimmed
            it_sel = it_sel
            i_report = i_report
            i_variant = i_variant ).

      CATCH cx_bapi_ex INTO DATA(error).
        error->get_text( ).

    ENDTRY.

  ENDMETHOD.

  METHOD start_parall_max_size.


    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_variant TYPE variant.

    it_sel = VALUE #( ( selname = 'P_JOBSUB'
                        kind    = 'P'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space )
                      ( selname = 'P_JOBNAM'
                        kind    = 'A'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space ) ).

    i_strtimmed = 'X'.
    i_report = 'ZVMA0_ARCH_EXTRACT'.
    i_variant = 'G1_2004_2009'.

    f_cut->it_jobs = VALUE #( ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ).


    f_cut->start_parallelism(
*       I_STRTIMMED = i_Strtimmed
        it_sel = it_sel
        i_report = i_report
        i_variant = i_variant ).

  ENDMETHOD.

  METHOD start_parall_not_max_size.

    DATA i_strtimmed TYPE btcchar1.
    DATA it_sel TYPE rsparams_tt.
    DATA i_report TYPE programm.
    DATA i_variant TYPE variant.

    it_sel = VALUE #( ( selname = 'P_JOBSUB'
                        kind    = 'P'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space )
                      ( selname = 'P_JOBNAM'
                        kind    = 'A'
                        sign    = 'I'
                        option  = 'EQ'
                        low     = abap_true
                        high    = space ) ).

    i_strtimmed = 'X'.
    i_report = 'ZHMA0_ARCH_EXTRACT'.
    i_variant = 'G1_2004_2009'.

    f_cut->it_jobs = VALUE #( ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' )
                              ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ( jobcount = '8' ) ).


    f_cut->start_parallelism(
*       I_STRTIMMED = i_Strtimmed
        it_sel = it_sel
        i_report = i_report
        i_variant = i_variant ).

  ENDMETHOD.

ENDCLASS.
