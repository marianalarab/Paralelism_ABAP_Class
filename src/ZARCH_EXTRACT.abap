REPORT ZARCH_EXTRACT.

DATA gs_vbak TYPE vbak.                                                           
DATA gs_vbap TYPE vbap.                                                                          
TYPES: BEGIN OF null,                                                                           
         vbeln TYPE vbuk-vbeln,                                                              
         lfstk TYPE vbuk-lfstk,                                                                  
         fkstk TYPE vbuk-fkstk,                                                                  
         gbstk TYPE vbuk-gbstk,                                                               
       END OF null.                                                                         
DATA: gs_vbuk_key_tmp TYPE vbuk_key,                                                        
      gs_vbuk_tmp TYPE vbuk.                                                                
TYPES: BEGIN OF ts_vbuk_sel,                                                                
      vbeln TYPE vbuk-vbeln,                                                                
      lfstk TYPE vbuk-lfstk,                                                                
      fkstk TYPE vbuk-fkstk,                                                                
      gbstk TYPE vbuk-gbstk,                                                                
       END OF ts_vbuk_sel.                                                                  
DATA: gt_vbuk TYPE STANDARD TABLE OF ts_vbuk_sel WITH EMPTY KEY,                            
      gt_vbuk_key_tmp TYPE vbuk_key_tab,                                                    
      gt_vbuk_tmp TYPE tdt_vbuk.                                                            


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_vbeln FOR gs_vbak-vbeln,                                                      
                s_erdat FOR gs_vbak-erdat,                                                      
                s_vbtyp FOR gs_vbak-vbtyp,                                                      
                s_auart FOR gs_vbak-auart,                                                      
                s_abgru FOR gs_vbap-abgru,                                                      
                s_vkorg FOR gs_vbak-vkorg,                                                      
                s_bukrs FOR gs_vbak-bukrs_vf.                                                   

PARAMETERS: p_jobsub TYPE c      NO-DISPLAY,
            p_jobnam TYPE btcjob NO-DISPLAY,
            filepath TYPE c LENGTH 130.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME.
PARAMETERS:
  p_clstbl TYPE c AS CHECKBOX USER-COMMAND cltb DEFAULT 'X'. " clean cluster table

SELECTION-SCREEN END OF BLOCK b02.

TYPES: BEGIN OF ty_vbak_sel,
         vbeln TYPE vbak-vbeln,
       END OF ty_vbak_sel.

TYPES: BEGIN OF ty_vbak,
         vbeln    TYPE vbak-vbeln,
         erdat    TYPE vbak-erdat,
         vbtyp    TYPE vbak-vbtyp,
         auart    TYPE vbak-auart,
         bukrs_vf TYPE vbak-bukrs_vf,
         bstnk    TYPE vbak-bstnk,
         vkorg    TYPE vbak-vkorg,
       END OF ty_vbak.

TYPES: BEGIN OF ty_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         vbtyp_n TYPE vbfa-vbtyp_n,
         erdat   TYPE vbfa-erdat,
         lgnum   TYPE vbfa-lgnum,
         mjahr   TYPE vbfa-mjahr,
         vbtyp_v TYPE vbfa-vbtyp_v,
       END OF ty_vbfa.

TYPES: BEGIN OF ty_vbap,
         vbeln    TYPE vbak-vbeln,
         posnr    TYPE vbap-posnr,
         erdat    TYPE vbak-erdat,
         vbtyp    TYPE vbak-vbtyp,
         auart    TYPE vbak-auart,
         bukrs_vf TYPE vbak-bukrs_vf,
         bstnk    TYPE vbak-bstnk,
         abgru    TYPE vbap-abgru,
         vkorg    TYPE vbak-vkorg,
         " ettyp    TYPE vbep-ettyp,
       END OF ty_vbap.

TYPES: BEGIN OF ty_vbep,
         vbeln TYPE vbep-vbeln,
         posnr TYPE vbep-posnr,
         ettyp TYPE vbep-ettyp,
         banfn TYPE vbep-banfn,
         bsart TYPE vbep-bsart,
         bnfpo TYPE vbep-bnfpo,
       END OF ty_vbep.

TYPES: BEGIN OF ty_result,
         vbeln    TYPE vbak-vbeln, "SD_VBAK - SALES ORDER
         posnr    TYPE vbap-posnr,
         erdat    TYPE vbak-erdat,
         vbtyp    TYPE vbak-vbtyp,
         auart    TYPE vbak-auart,
         bukrs_vf TYPE vbak-bukrs_vf,
         vkorg    TYPE vbak-vkorg,
         bstnk    TYPE vbak-bstnk,
         abgru    TYPE vbap-abgru,
         vbtyp_n  TYPE vbfa-vbtyp_n,
         vbelns   TYPE vbfa-vbeln,
         posnn    TYPE vbfa-posnn,
         status   TYPE c LENGTH 1,
         mjahr    TYPE vbfa-mjahr,
         erdats   TYPE vbak-erdat,
         tknum    TYPE vttk-tknum, "SD_VTTK - SHIPMENT
         8erdat   TYPE vttk-erdat,
         8stat    TYPE c LENGTH 1,
         fknum    TYPE vfkk-fknum, "SD_VFKK - SHIPMENT COST
         aerdat   TYPE vfkk-erdat,
         ettyp    TYPE vbep-ettyp,
         lblni    TYPE essr-lblni, "Entry Sheet
         lerdat   TYPE essr-erdat,
         banfn    TYPE vbep-banfn, "MM_EBAN - PURCHASE REQUISITION
         bnfpo    TYPE vbep-bnfpo,
         bsart    TYPE vbep-bsart,
         req_date TYPE eban-erdat,
         rloekz   TYPE eban-loekz, "Deletion Ind.
         ploekz   TYPE ekko-loekz, "Deletion Ind.
         lgnum    TYPE ltak-lgnum, "Warehouse No. for Transfer Order
         ostatus  TYPE gbstk,                                                                   
         ebeln    TYPE essr-ebeln, "PO for the Entry Sheet
       END OF ty_result.

TYPES: BEGIN OF ty_download,
         line TYPE string,
       END OF ty_download.

DATA: it_vbak       TYPE TABLE OF ty_vbak,
      t_vbak_sel    TYPE TABLE OF ty_vbak_sel,
      it_vbfa       TYPE TABLE OF ty_vbfa,
      it_vbfa_aux   TYPE TABLE OF ty_vbfa,
      it_vbap       TYPE TABLE OF ty_vbap,
      it_vbep       TYPE TABLE OF ty_vbep,
      it_result     TYPE TABLE OF ty_result,
      it_result_aux TYPE TABLE OF ty_result,
      it_download   TYPE TABLE OF ty_download.

DATA   r_ettyp   TYPE RANGE OF ettyp.
TYPES: r_range_t TYPE RANGE OF ettyp.
DATA:  s_del TYPE RANGE OF vbak-vbeln.

DATA: lc_where   TYPE string.
DATA: lc_field   TYPE string.
DATA: lc_field2   TYPE string.

DATA: it_sel TYPE TABLE OF rsparams.

DATA: v_count TYPE i.

DATA: filename TYPE string.

DATA: result TYPE string.

FIELD-SYMBOLS: <fs_result> TYPE ty_result.

DATA: ls_result TYPE ty_result.

DATA: s_tanum TYPE RANGE OF ltak-tanum,
      v_tanum TYPE ltak-tanum.


DATA: v_target_date TYPE syst-datum.

START-OF-SELECTION.

  DATA(o_parallel) = NEW ZCL_PARALLEL_JOB(  ).

  IF p_jobsub IS INITIAL.
    "Main Job execution

* Clean cluster table for SD program (in case the program suffered from a dump)
    IF p_clstbl IS NOT INITIAL.

      DATA(lv_jobname) = sy-repid(17) && '%'.

      SELECT DISTINCT srtfd FROM zparallel_cluster
        INTO TABLE @DATA(lt_jobname)
        WHERE relid = 'ST'
          AND srtfd LIKE @lv_jobname.


      LOOP AT lt_jobname INTO DATA(wa_jobname).

        TRY .

            cl_abap_expimp_utilities=>db_delete(
               EXPORTING
                 tabname             = 'ZPARALLEL_CLUSTER'
                 client              = sy-mandt
                 area                = 'ST'
                 id                  = wa_jobname-srtfd
                 generic_key         = abap_false
                 client_specified    = abap_true
             ).
          CATCH cx_sy_client.    "
            WRITE: / 'cl_abap_expimp_utilities=>db_delete - cx_sy_client'.
          CATCH cx_sy_generic_key.    "
            WRITE: / 'cl_abap_expimp_utilities=>db_delete - cx_sy_generic_key'.
          CATCH cx_sy_incorrect_key.    "
            WRITE: / 'cl_abap_expimp_utilities=>db_delete - cx_sy_incorrect_key'.

        ENDTRY.

        COMMIT WORK.

      ENDLOOP.

    ENDIF.

    IF NOT s_abgru IS INITIAL.

      SELECT vbak~vbeln,
             vbak~erdat,
             vbak~vbtyp,
             vbak~auart,
             vbak~bukrs_vf,
             vbak~bstnk,
             vbak~vkorg
      FROM vbak
      INNER JOIN vbap
      ON vbak~vbeln EQ vbap~vbeln
      INTO TABLE @it_vbak
      WHERE vbak~vbeln    IN @s_vbeln  AND
            vbak~erdat    IN @s_erdat  AND
            vbak~vbtyp    IN @s_vbtyp  AND
            vbak~auart    IN @s_auart  AND
            vbak~vkorg    IN @s_vkorg  AND
            vbak~bukrs_vf IN @s_bukrs  AND
            vbap~abgru    IN @s_abgru.

      IF sy-subrc EQ 0.
        SORT it_vbak BY vbeln ASCENDING.
      ENDIF.

    ELSE.

      SELECT vbeln
             erdat
             vbtyp
             auart
             bukrs_vf
             bstnk
             vkorg
          FROM vbak
          INTO TABLE it_vbak
          WHERE vbeln IN s_vbeln  AND
                erdat IN s_erdat  AND
                vbtyp IN s_vbtyp  AND
                auart IN s_auart  AND
                vkorg IN s_vkorg  AND
                bukrs_vf IN s_bukrs
                ORDER BY PRIMARY KEY.

    ENDIF.

    LOOP AT it_vbak ASSIGNING FIELD-SYMBOL(<fs_vbakbstnk>).
      REPLACE ALL OCCURRENCES OF SUBSTRING ';' IN <fs_vbakbstnk>-bstnk WITH ''.
    ENDLOOP.


    CLEAR it_sel.
    LOOP AT it_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>).

      APPEND INITIAL LINE TO it_sel ASSIGNING FIELD-SYMBOL(<fl_sel>).
      <fl_sel>-selname = 'S_VBELN'.
      <fl_sel>-kind = 'S'.
      <fl_sel>-low = <fs_vbak>-vbeln.
      <fl_sel>-sign = 'I'.
      <fl_sel>-option = 'EQ'.

    ENDLOOP.


    "start all other sub jobs with submit to itself.
    o_parallel->start_parallelism(
      EXPORTING
        it_sel      = it_sel
        i_report    = sy-repid
        i_variant   = sy-slset
    ).

    CLEAR: it_result, it_result_aux.

    DATA(it_jobs) = o_parallel->get_all_jobs( ).

    LOOP AT it_jobs ASSIGNING FIELD-SYMBOL(<fs_jobs>).

      "Import data from cluster table
      o_parallel->import_data(
        EXPORTING
          i_name  = <fs_jobs>-jobname
        IMPORTING
          t_table = it_result_aux
      ).

      APPEND LINES OF it_result_aux TO it_result.
      CLEAR it_result_aux.

    ENDLOOP.

    COMMIT WORK.

    CLEAR it_download.
    LOOP AT it_result ASSIGNING <fs_result>.

      IF sy-tabix = 1.
        APPEND VALUE #( line =  'Sales Order'          && ';' &&
                                'SO ITEM'              && ';' &&
                                'SO DATE'              && ';' &&
                                'Sales Order Status'   && ';' &&
                                'Document cat.'        && ';' &&
                                'Sales Doc. Type'      && ';' &&
                                'CCode'                && ';' &&
                                'Sales Org'            && ';' &&
                                'Leg PO Number'        && ';' &&
                                'RejectionReason'      && ';' &&
                                'Subsequent doc.'      && ';' &&
                                'Subsequent item'      && ';' &&
                                'Subsequent doc. type' && ';' &&
                                'Status'               && ';' &&
                                'Subs. Doc Date'       && ';' &&
                                'Shipment'             && ';' &&
                                'Shipment Date'        && ';' &&
                                'Shipment Status'      && ';' &&
                                'Mat. Doc. Year'       && ';' &&
                                'Shipmt Cost No.'      && ';' &&
                                'Shipmt Cost Date'     && ';' &&
                                'Sched.line cat.'      && ';' &&
                                'Entry Sheet'          && ';' &&
                                'Entry Sheet Date'     && ';' &&
                                'Purchase Req.'        && ';' &&
                                'Requisn Item'         && ';' &&
                                'Req Type'             && ';' &&
                                'Req date'             && ';' &&
                                'Req Deletion Ind'     && ';' &&
                                'PO Deletion Ind'      && ';' &&
                                'Warehouse No.'        && ';' &&
                                'PO Entry Sheet'
                                ) TO it_download.

      ENDIF.

      CASE <fs_result>-vbtyp_n.
        WHEN IF_SD_DOC_CATEGORY=>CANCEL_GOODS_MOVEMENT.                                          
          DATA(v_desc) = 'Cancel Goods Issue'.
        WHEN IF_SD_DOC_CATEGORY=>GOODS_RECEIPT OR IF_SD_DOC_CATEGORY=>GOODS_MOVEMENT.            
          "Doc. Material                                                                         
          v_desc = 'Doc. Material'.
        WHEN IF_SD_DOC_CATEGORY=>SHIPMENT. "Shipment                                             
          v_desc = 'Shipment'.
        WHEN IF_SD_DOC_CATEGORY=>DELIVERY OR IF_SD_DOC_CATEGORY=>RETURNS_DELIVERY_FOR_ORDER.     
          "Delivery                                                                              
          v_desc = 'Delivery'.
        WHEN IF_SD_DOC_CATEGORY=>INVOICE OR IF_SD_DOC_CATEGORY=>INVOICE_CANCEL OR                
          IF_SD_DOC_CATEGORY=>DEBIT_MEMO OR IF_SD_DOC_CATEGORY=>CREDIT_MEMO. "Billing            
          v_desc = 'Billing'.
        WHEN IF_SD_DOC_CATEGORY=>PURCHASE_ORDER.                                                 
          v_desc = 'Purchase Order'.
        WHEN IF_SD_DOC_CATEGORY=>WMS_TRANS_ORDER.                                                
          v_desc = 'WMS transfer order'.
        WHEN IF_SD_DOC_CATEGORY=>ORDER OR IF_SD_DOC_CATEGORY=>RETURNS.                           
          v_desc = 'Sales Order'.
        WHEN IF_SD_DOC_CATEGORY=>PURCHASE_ORDER.                                                 
          v_desc = 'Purchase Order'.
      ENDCASE.


      CASE <fs_result>-status.
        WHEN 'C'.
          DATA(v_status) = 'Completely processed'.
        WHEN 'A'.
          v_status = 'Not yet processed'.
        WHEN 'B'.
          v_status = 'Partially processed'.
        WHEN ''.
          v_status = 'Not Relevant'.
      ENDCASE.

      CASE <fs_result>-ostatus.
        WHEN 'C'.
          DATA(v_ostatus) = 'Completely processed'.
        WHEN 'A'.
          v_ostatus = 'Not yet processed'.
        WHEN 'B'.
          v_ostatus = 'Partially processed'.
        WHEN ''.
          v_ostatus = 'Not Relevant'.
      ENDCASE.


      CASE <fs_result>-8stat.
        WHEN 'C'.
          DATA(v_8stat) = 'Completely Processed'.
        WHEN ''.
          v_8stat = 'Not Relevant'.
        WHEN 'A'.
          v_8stat = 'Not Processed'.
        WHEN 'B'.
          v_8stat = 'Partially Processed'.
      ENDCASE.

      APPEND VALUE #( line =  <fs_result>-vbeln         && ';' &&
                              <fs_result>-posnr         && ';' &&
                              <fs_result>-erdat+4(2)    && '/' &&  <fs_result>-erdat+6(2) && '/' &&  <fs_result>-erdat(4) && ';' &&
                              v_ostatus                 && ';' &&
                              <fs_result>-vbtyp         && ';' &&
                              <fs_result>-auart         && ';' &&
                              <fs_result>-bukrs_vf      && ';' &&
                              <fs_result>-vkorg         && ';' &&
                              <fs_result>-bstnk         && ';' &&
                              <fs_result>-abgru         && ';' &&
                              <fs_result>-vbelns        && ';' &&
                              <fs_result>-posnn         && ';' &&
                              v_desc                    && ';' &&
                              v_status                  && ';' &&
                              <fs_result>-erdats+4(2)   && '/' &&  <fs_result>-erdats+6(2) && '/' &&  <fs_result>-erdats(4) && ';' &&
                              <fs_result>-tknum         && ';' &&
                              <fs_result>-8erdat+4(2)   && '/' &&  <fs_result>-8erdat+6(2) && '/' &&  <fs_result>-8erdat(4) && ';' &&
                              v_8stat                   && ';' &&
                              <fs_result>-mjahr         && ';' &&
                              <fs_result>-fknum         && ';' &&
                              <fs_result>-aerdat+4(2)   && '/' &&  <fs_result>-aerdat+6(2) && '/' &&  <fs_result>-aerdat(4) && ';' &&
                              <fs_result>-ettyp         && ';' &&
                              <fs_result>-lblni         && ';' &&
                              <fs_result>-lerdat+4(2)   && '/' &&  <fs_result>-lerdat+6(2) && '/' &&  <fs_result>-lerdat(4) && ';' &&
                              <fs_result>-banfn         && ';' &&
                              <fs_result>-bnfpo         && ';' &&
                              <fs_result>-bsart         && ';' &&
                              <fs_result>-req_date+4(2) && '/' &&  <fs_result>-req_date+6(2)  && '/' && <fs_result>-req_date(4)  && ';' &&
                              <fs_result>-rloekz        && ';' &&
                              <fs_result>-ploekz        && ';' &&
                              <fs_result>-lgnum         && ';' &&
                              <fs_result>-ebeln
                              ) TO it_download.

      CLEAR: v_ostatus, v_desc, v_status, v_8stat.

    ENDLOOP.

    filename = filepath.

    IF sy-batch IS INITIAL.

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename                  =  filename   " Name of file
        CHANGING
          data_tab                  =  it_download   " Transfer table
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          not_supported_by_gui      = 22
          error_no_gui              = 23
          OTHERS                    = 24
      ).
      IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.


  ELSE.


    v_target_date = '20170101'.

    CLEAR: it_vbfa, it_vbap.

    "Sales Document Flow
    SELECT vbelv
           posnv
           vbeln
           posnn
           vbtyp_n
           erdat
           lgnum
           mjahr
           vbtyp_v
      FROM vbfa
      INTO TABLE it_vbfa
      WHERE vbelv IN s_vbeln.

    IF sy-subrc EQ 0.

      SORT it_vbfa ASCENDING BY vbelv posnv.

      it_vbfa_aux = it_vbfa.
      DELETE it_vbfa_aux WHERE vbtyp_v NE if_sd_doc_category=>sched_agree.                 

      "Sales Document Flow Schedule Agrements - E

      IF NOT it_vbfa_aux IS INITIAL .

        SELECT DISTINCT b~vbelv
                        b~posnv
                        a~vbeln
                        a~posnn
                        a~vbtyp_n
                        a~erdat
                        a~lgnum
                        a~mjahr
        FROM vbfa AS a
        INNER JOIN vbfa AS b
        ON a~vbelv = b~vbeln
        APPENDING TABLE it_vbfa
        FOR ALL ENTRIES IN it_vbfa_aux
        WHERE a~vbelv = it_vbfa_aux-vbeln
          AND a~posnv = it_vbfa_aux-posnn.

        SORT it_vbfa ASCENDING BY vbelv posnv.

      ENDIF.

      "Return Order - H

      it_vbfa_aux = it_vbfa.
      DELETE it_vbfa_aux WHERE vbtyp_n NE if_sd_doc_category=>returns.                        

      IF NOT it_vbfa_aux IS INITIAL.

        SELECT vbelv
               posnv
               vbeln
               posnn
               vbtyp_n
               erdat
               lgnum
               mjahr
        FROM vbfa
        APPENDING TABLE it_vbfa
        FOR ALL ENTRIES IN it_vbfa_aux
        WHERE vbelv = it_vbfa_aux-vbeln
          AND posnv = it_vbfa_aux-posnn.

        LOOP AT it_vbfa_aux ASSIGNING FIELD-SYMBOL(<fs_vbfa_aux>).

          APPEND INITIAL LINE TO s_vbeln ASSIGNING FIELD-SYMBOL(<fs_sebeln>).
          <fs_sebeln>-low = <fs_vbfa_aux>-vbeln.
          <fs_sebeln>-sign = 'I'.
          <fs_sebeln>-option = 'EQ'.

        ENDLOOP.

        SORT it_vbfa ASCENDING BY vbelv posnv.

      ENDIF.
    ENDIF.

    IF NOT s_vbeln IS INITIAL.

      SELECT k~vbeln
             p~posnr
             k~erdat
             k~vbtyp
             k~auart
             k~bukrs_vf
             k~bstnk
             p~abgru
             k~vkorg
      FROM vbak AS k
      INNER JOIN vbap AS p
      ON k~vbeln EQ p~vbeln
      INTO TABLE it_vbap
      WHERE k~vbeln IN s_vbeln
        AND k~erdat IN s_erdat
        AND k~vbtyp IN s_vbtyp
        AND k~auart IN s_auart
        AND k~bukrs_vf IN s_bukrs.

      IF sy-subrc IS INITIAL.

        LOOP AT it_vbap ASSIGNING FIELD-SYMBOL(<fs_vbapbstnk>).
          REPLACE ALL OCCURRENCES OF SUBSTRING ';' IN <fs_vbapbstnk>-bstnk WITH ''.
        ENDLOOP.

        SORT it_vbap BY vbeln posnr.

        LOOP AT it_vbap ASSIGNING FIELD-SYMBOL(<gv_vbuk_tmp>).                                   "$smart: #612
          gs_vbuk_key_tmp-vbeln = <gv_vbuk_tmp>-vbeln.                                           "$smart: #612
          INSERT gs_vbuk_key_tmp INTO TABLE gt_vbuk_key_tmp.                                     "$smart: #612
        ENDLOOP.                                                                                 "$smart: #612
        CALL FUNCTION 'SD_VBUK_READ_FROM_DOC_MULTI'                                              "$smart: #612
          EXPORTING                                                                              "$smart: #612
            it_vbuk_key         = gt_vbuk_key_tmp                                                "$smart: #612
          IMPORTING                                                                              "$smart: #612
            et_vbuk             = gt_vbuk_tmp                                                    "$smart: #612
          EXCEPTIONS                                                                             "$smart: #612
            vbeln_not_found     = 1                                                              "$smart: #612
            vbtyp_not_supported = 2                                                              "$smart: #612
            vbobj_not_supported = 3                                                              "$smart: #612
            others              = 4.                                                             "$smart: #612
        IF sy-subrc EQ 0.                                                                        "$smart: #612
          CLEAR gt_vbuk[].                                                                       "$smart: #612
          LOOP AT gt_vbuk_tmp ASSIGNING FIELD-SYMBOL(<gs_vbuk_tmp_1>).                           "$smart: #612
            gs_vbuk_tmp-vbeln = <gs_vbuk_tmp_1>-vbeln.                                           "$smart: #612
            gs_vbuk_tmp-lfstk = <gs_vbuk_tmp_1>-lfstk.                                           "$smart: #612
            gs_vbuk_tmp-fkstk = <gs_vbuk_tmp_1>-fkstk.                                           "$smart: #612
            gs_vbuk_tmp-gbstk = <gs_vbuk_tmp_1>-gbstk.                                           "$smart: #612
            APPEND gs_vbuk_tmp TO gt_vbuk[].                                                     "$smart: #612
          ENDLOOP.                                                                               "$smart: #612
          CLEAR gt_vbuk_tmp.                                                                     "$smart: #612
        ENDIF.                                                                                   "$smart: #612
        FREE gt_vbuk_key_tmp.                                                                    "$smart: #612
        r_ettyp = VALUE r_range_t(
                  LET s = 'I'
                      o = 'EQ'
                  IN sign   = s
                     option = o
                     ( low = 'CB' )
                     ( low = 'YC' )
                     ( low = 'YI' )
                     ( low = 'Z1' )
                     ( low = 'Z2' )
                     ( low = 'Z6' )
                     ( low = 'Z7' )
                     ( low = 'Z9' )
                     ( low = 'ZB' )
                     ( low = 'ZG' )
                     ( low = 'ZL' )
                     ( low = 'ZP' )
                     ( low = 'ZV' )
                     ( low = 'ZW' )
                     ( low = 'ZX' )
        ).

        SELECT vbeln
               posnr
               ettyp
               banfn
               bsart
               bnfpo
          FROM vbep
          INTO TABLE it_vbep
          FOR ALL ENTRIES IN it_vbap
          WHERE vbeln = it_vbap-vbeln
            AND posnr = it_vbap-posnr
            AND ettyp IN r_ettyp.
        SORT it_vbep BY vbeln posnr. 

        IF sy-subrc IS INITIAL.

          SELECT banfn, bnfpo, loekz, erdat
          FROM eban
          INTO TABLE @DATA(it_eban)
          FOR ALL ENTRIES IN @it_vbep
          WHERE banfn EQ @it_vbep-banfn
            AND bnfpo EQ @it_vbep-bnfpo
            ORDER BY PRIMARY KEY.

        ENDIF.

      ENDIF.

      "Document flow
      IF NOT it_vbfa IS INITIAL.

        "Material Documnent check
        it_vbfa_aux = it_vbfa.
        DELETE it_vbfa_aux WHERE vbtyp_n NE if_sd_doc_category=>goods_receipt AND vbtyp_n NE     
          if_sd_doc_category=>goods_movement AND vbtyp_n NE                                      
          if_sd_doc_category=>cancel_goods_movement.                                             

        IF NOT it_vbfa_aux IS INITIAL.

          SELECT mblnr, mjahr, budat
          FROM mkpf
          INTO TABLE @DATA(it_mkpf)
          FOR ALL ENTRIES IN @it_vbfa_aux
          WHERE mblnr = @it_vbfa_aux-vbeln AND
                mjahr = @it_vbfa_aux-mjahr
                ORDER BY PRIMARY KEY.

        ENDIF.

        "Delivery and "Return Delivery - T
        it_vbfa_aux = it_vbfa.
        DELETE it_vbfa_aux WHERE vbtyp_n NE if_sd_doc_category=>delivery AND vbtyp_n NE         
          if_sd_doc_category=>returns_delivery_for_order.                                       
        IF NOT it_vbfa_aux IS INITIAL.

          SELECT vbeln, erdat
          FROM likp
          INTO TABLE @DATA(it_likp)
          FOR ALL ENTRIES IN @it_vbfa_aux
          WHERE vbeln EQ @it_vbfa_aux-vbeln.

          SORT it_likp ASCENDING BY vbeln.

          "Shipment
          SELECT vbelv, posnv, vbeln, posnn
             FROM vbfa
             INTO TABLE @DATA(it_vbfa_ship)
             FOR ALL ENTRIES IN @it_vbfa_aux
             WHERE vbelv = @it_vbfa_aux-vbeln
               AND posnv = @space
               AND vbtyp_n = @if_sd_doc_category=>shipment.                                      "$smart: #607

          IF sy-subrc EQ 0.

            SORT it_vbfa_ship ASCENDING BY vbelv posnv.

            IF NOT it_vbfa_ship IS INITIAL.

              SELECT tknum, erdat, fbgst
              FROM vttk
              INTO TABLE @DATA(it_vttk)
              FOR ALL ENTRIES IN @it_vbfa_ship
              WHERE tknum EQ @it_vbfa_ship-vbeln
              ORDER BY PRIMARY KEY.

              IF sy-subrc EQ 0.
                "Shipment Cost
                SELECT fknum, fkpos, rebel, erdat
                FROM vfkp
                INTO TABLE @DATA(it_vfkp)
                FOR ALL ENTRIES IN @it_vttk
                WHERE rebel EQ @it_vttk-tknum.

                IF sy-subrc EQ 0.

                  SORT it_vfkp BY rebel ASCENDING.

                  "Entry Sheet
                  SELECT lblni, erdat, fknum, fkpos, ebeln
                  FROM essr
                  INTO TABLE @DATA(it_essr)
                  FOR ALL ENTRIES IN @it_vfkp
                  WHERE  fknum = @it_vfkp-fknum
                    AND  fkpos = @it_vfkp-fkpos.

                  IF sy-subrc IS INITIAL.
                    SORT it_essr ASCENDING BY fknum fkpos.
                  ENDIF.
                ENDIF.

              ENDIF.

            ENDIF.
          ENDIF.

        ENDIF.

        "Billing
        it_vbfa_aux = it_vbfa.
        DELETE it_vbfa_aux WHERE vbtyp_n NE if_sd_doc_category=>invoice AND vbtyp_n NE           
          if_sd_doc_category=>invoice_cancel AND vbtyp_n NE if_sd_doc_category=>debit_memo AND   
          vbtyp_n NE if_sd_doc_category=>credit_memo AND vbtyp_n NE                              
          if_sd_doc_category=>intercompany_invoice AND vbtyp_n NE                                
          if_sd_doc_category=>pro_forma_invoice. "Billing                                        
        IF NOT it_vbfa_aux IS INITIAL.

          SELECT vbeln, erdat
          FROM vbrk
          INTO TABLE @DATA(it_vbrk)
          FOR ALL ENTRIES IN @it_vbfa_aux
          WHERE vbeln EQ @it_vbfa_aux-vbeln                                                      
  . "#EC CI_DB_OPERATION_OK[2768887]                                                             

          SORT it_vbrk ASCENDING BY vbeln.

        ENDIF.

        "Purchase Order
        it_vbfa_aux = it_vbfa.
        DELETE it_vbfa_aux WHERE vbtyp_n NE if_sd_doc_category=>purchase_order.                  
        IF NOT it_vbfa_aux IS INITIAL.

          SELECT ebeln, aedat, loekz
          FROM ekko
          INTO TABLE @DATA(it_ekko)
          FOR ALL ENTRIES IN @it_vbfa_aux
          WHERE ebeln EQ @it_vbfa_aux-vbeln.

          SORT it_ekko ASCENDING BY ebeln.

        ENDIF.


        "WM Transfer Order
        it_vbfa_aux = it_vbfa.
        DELETE it_vbfa_aux WHERE vbtyp_n NE if_sd_doc_category=>wms_trans_order.                
        IF NOT it_vbfa_aux IS INITIAL.

          DATA(lt_ltak) = VALUE tt_ltak( FOR wa IN it_vbfa_aux ( lgnum = wa-lgnum tanum = wa-vbeln ) ).
          SELECT lgnum, tanum, bdatu
          FROM ltak
          INTO TABLE @DATA(it_ltak)
          FOR ALL ENTRIES IN @lt_ltak
          WHERE lgnum EQ @lt_ltak-lgnum
            AND tanum EQ @lt_ltak-tanum
            ORDER BY PRIMARY KEY.


        ENDIF.

      ENDIF.

    ENDIF.

************************************************************************************************************

    LOOP AT it_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>).

      READ TABLE gt_vbuk INTO DATA(s_vbuk) WITH KEY vbeln = <fs_vbap>-vbeln BINARY SEARCH.       

      READ TABLE it_vbfa TRANSPORTING NO FIELDS WITH KEY vbelv = <fs_vbap>-vbeln
                                                         posnv = <fs_vbap>-posnr
                                                         BINARY SEARCH.
      IF sy-subrc EQ 0.

        LOOP AT it_vbfa FROM sy-tabix ASSIGNING FIELD-SYMBOL(<fs_vbfa>).

          IF <fs_vbap>-vbeln   NE <fs_vbfa>-vbelv OR
             <fs_vbap>-posnr   NE <fs_vbfa>-posnv.
            EXIT.
          ENDIF.

          CLEAR ls_result.
          ls_result-vbelns   = <fs_vbfa>-vbeln.
          ls_result-vbeln    = <fs_vbap>-vbeln.
          ls_result-posnr    = <fs_vbap>-posnr.
          ls_result-erdat    = <fs_vbap>-erdat.
          ls_result-vbtyp    = <fs_vbap>-vbtyp.
          ls_result-auart    = <fs_vbap>-auart.
          ls_result-bukrs_vf = <fs_vbap>-bukrs_vf.
          ls_result-bstnk    = <fs_vbap>-bstnk.
          ls_result-abgru    = <fs_vbap>-abgru.
          ls_result-vbtyp_n  = <fs_vbfa>-vbtyp_n.
          ls_result-posnn    = <fs_vbfa>-posnn.
          ls_result-vkorg    = <fs_vbap>-vkorg.
          ls_result-ostatus  = s_vbuk-gbstk.

          READ TABLE it_vbep ASSIGNING FIELD-SYMBOL(<fs_vbep>) WITH KEY vbeln = <fs_vbap>-vbeln
                                                                        posnr = <fs_vbap>-posnr
                                                                        BINARY SEARCH.

          IF sy-subrc IS INITIAL.

            READ TABLE it_eban ASSIGNING FIELD-SYMBOL(<fs_eban>) WITH KEY banfn = <fs_vbep>-banfn
                                                                          bnfpo = <fs_vbep>-bnfpo
                                                                          BINARY SEARCH.
            IF sy-subrc EQ 0.

              ls_result-ettyp    = <fs_vbep>-ettyp.
              ls_result-banfn    = <fs_vbep>-banfn.
              ls_result-bsart    = <fs_vbep>-bsart.
              ls_result-bnfpo    = <fs_vbep>-bnfpo.
              ls_result-req_date = <fs_eban>-erdat.
              ls_result-rloekz   = <fs_eban>-loekz.


            ENDIF.
          ENDIF.

          CASE  <fs_vbfa>-vbtyp_n .
            WHEN IF_SD_DOC_CATEGORY=>GOODS_RECEIPT OR IF_SD_DOC_CATEGORY=>GOODS_MOVEMENT OR      
              IF_SD_DOC_CATEGORY=>CANCEL_GOODS_MOVEMENT. "Doc. Material                          

              ls_result-mjahr = <fs_vbfa>-mjahr.
              READ TABLE it_mkpf ASSIGNING FIELD-SYMBOL(<fs_mkpf>) WITH KEY mblnr = <fs_vbfa>-vbeln
                                                                            mjahr  = <fs_vbfa>-posnn
                                                                            BINARY SEARCH.
              IF sy-subrc EQ 0.

                ls_result-erdats = <fs_mkpf>-budat.

              ELSE.

                CONTINUE.

              ENDIF.


            WHEN IF_SD_DOC_CATEGORY=>DELIVERY OR IF_SD_DOC_CATEGORY=>RETURNS_DELIVERY_FOR_ORDER.
              "Delivery                                                                         

              READ TABLE it_likp ASSIGNING FIELD-SYMBOL(<fs_likp>) WITH KEY vbeln = <fs_vbfa>-vbeln BINARY SEARCH.
              IF sy-subrc EQ 0.

                ls_result-erdats = <fs_likp>-erdat.
                ls_result-status = s_vbuk-lfstk.

              ELSE.

                CONTINUE.

              ENDIF.

              "Shipment
              READ TABLE it_vbfa_ship TRANSPORTING NO FIELDS WITH KEY vbelv = <fs_vbfa>-vbeln BINARY SEARCH.
              IF sy-subrc EQ 0.

                LOOP AT it_vbfa_ship ASSIGNING FIELD-SYMBOL(<fs_vbfa_ship>) FROM sy-tabix.

                  IF <fs_vbfa_ship>-vbelv NE <fs_vbfa>-vbeln.
                    EXIT.
                  ENDIF.


                  READ TABLE it_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>) WITH KEY tknum = <fs_vbfa_ship>-vbeln BINARY SEARCH.
                  IF sy-subrc EQ 0.

                    ls_result-tknum  = <fs_vttk>-tknum.
                    ls_result-8erdat = <fs_vttk>-erdat.
                    ls_result-8stat = <fs_vttk>-fbgst.

                    READ TABLE it_vfkp ASSIGNING FIELD-SYMBOL(<fs_vfkp>) WITH KEY rebel = <fs_vttk>-tknum BINARY SEARCH.
                    IF sy-subrc EQ 0.

                      ls_result-fknum = <fs_vfkp>-fknum.
                      ls_result-aerdat = <fs_vfkp>-erdat.

                      READ TABLE  it_essr ASSIGNING FIELD-SYMBOL(<fs_essr>) WITH KEY fknum = <fs_vfkp>-fknum
                                                                                     fkpos = <fs_vfkp>-fkpos
                                                                                     BINARY SEARCH.
                      IF sy-subrc EQ 0.

                        ls_result-lblni = <fs_essr>-lblni.
                        ls_result-lerdat = <fs_essr>-erdat.
                        ls_result-ebeln = <fs_essr>-ebeln.

                      ENDIF.

                    ENDIF.

                  ENDIF.

                  APPEND ls_result TO it_result.

                ENDLOOP.

                CONTINUE.

              ENDIF.

            WHEN IF_SD_DOC_CATEGORY=>INVOICE OR IF_SD_DOC_CATEGORY=>INVOICE_CANCEL OR            
              IF_SD_DOC_CATEGORY=>DEBIT_MEMO OR IF_SD_DOC_CATEGORY=>CREDIT_MEMO OR               
              IF_SD_DOC_CATEGORY=>INTERCOMPANY_INVOICE OR IF_SD_DOC_CATEGORY=>PRO_FORMA_INVOICE. 
              "Billing                                                                           

              READ TABLE it_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>) WITH KEY vbeln = <fs_vbfa>-vbeln BINARY SEARCH.
              IF sy-subrc EQ 0.

                ls_result-erdats = <fs_vbrk>-erdat.
                ls_result-status = s_vbuk-fkstk.

              ELSE.

                CONTINUE.

              ENDIF.

            WHEN IF_SD_DOC_CATEGORY=>PURCHASE_ORDER. "Purchase Order                          

              READ TABLE it_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>) WITH KEY ebeln = <fs_vbfa>-vbeln
                                                                            BINARY SEARCH.
              IF sy-subrc EQ 0.

                ls_result-vbelns = <fs_ekko>-ebeln.
                ls_result-erdats = <fs_ekko>-aedat.
                ls_result-ploekz = <fs_ekko>-loekz.

              ELSE.

                CONTINUE.

              ENDIF.

            WHEN IF_SD_DOC_CATEGORY=>WMS_TRANS_ORDER. "WMS transfer order                       

              v_tanum = <fs_vbfa>-vbeln.
              READ TABLE it_ltak ASSIGNING FIELD-SYMBOL(<fs_ltak>) WITH KEY lgnum = <fs_vbfa>-lgnum
                                                                            tanum = v_tanum
                                                                            BINARY SEARCH.
              IF sy-subrc EQ 0.

                ls_result-erdats = <fs_ltak>-bdatu.
                ls_result-lgnum  = <fs_ltak>-lgnum.

              ELSE.

                CONTINUE.

              ENDIF.

          ENDCASE.

          APPEND ls_result TO it_result.

        ENDLOOP.

      ENDIF. "VBFA

      READ TABLE it_result TRANSPORTING NO FIELDS WITH KEY vbeln   = <fs_vbap>-vbeln
                                                           posnr   = <fs_vbap>-posnr
                                                           vbtyp_n = '' .
      IF sy-subrc NE 0.

        CLEAR ls_result.
        ls_result-vbelns   = <fs_vbap>-vbeln.
        ls_result-vbeln    = <fs_vbap>-vbeln.
        ls_result-posnr    = <fs_vbap>-posnr.
        ls_result-erdat    = <fs_vbap>-erdat.
        ls_result-vbtyp    = <fs_vbap>-vbtyp.
        ls_result-auart    = <fs_vbap>-auart.
        ls_result-bukrs_vf = <fs_vbap>-bukrs_vf.
        ls_result-bstnk    = <fs_vbap>-bstnk.
        ls_result-abgru    = <fs_vbap>-abgru.
        ls_result-ostatus  = s_vbuk-gbstk.

        READ TABLE it_vbep ASSIGNING <fs_vbep> WITH KEY vbeln = <fs_vbap>-vbeln
                                                        posnr = <fs_vbap>-posnr
                                                        BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          READ TABLE it_eban ASSIGNING <fs_eban> WITH KEY banfn = <fs_vbep>-banfn
                                                          bnfpo = <fs_vbep>-bnfpo
                                                          BINARY SEARCH.
          IF sy-subrc EQ 0.

            ls_result-ettyp    = <fs_vbep>-ettyp.
            ls_result-banfn    = <fs_vbep>-banfn.
            ls_result-bsart    = <fs_vbep>-bsart.
            ls_result-bnfpo    = <fs_vbep>-bnfpo.
            ls_result-req_date = <fs_eban>-erdat.
            ls_result-rloekz   = <fs_eban>-loekz.


          ENDIF.
        ENDIF.

        APPEND ls_result TO it_result.

      ENDIF.

    ENDLOOP. "VBAP

    "Check Date
    LOOP AT it_result ASSIGNING <fs_result>.

      IF <fs_result>-erdat    GE v_target_date OR
         <fs_result>-erdats   GE v_target_date OR
         <fs_result>-8erdat   GE v_target_date OR
         <fs_result>-aerdat   GE v_target_date OR
         <fs_result>-lerdat   GE v_target_date OR
         <fs_result>-req_date GE v_target_date.

        APPEND INITIAL LINE TO s_del ASSIGNING FIELD-SYMBOL(<fs_del>).
        <fs_del>-low    = <fs_result>-vbeln.
        <fs_del>-sign   = 'I'.
        <fs_del>-option = 'EQ'.

      ENDIF.

    ENDLOOP.

    IF NOT s_del IS INITIAL.

      DELETE it_result WHERE vbeln IN s_del.

    ENDIF.


    o_parallel->export_data(
      EXPORTING
        i_name  = p_jobnam
        t_table = it_result
    ).


  ENDIF.
