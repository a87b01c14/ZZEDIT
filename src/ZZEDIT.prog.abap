* Copyright (C) 2024 ABAP三叔 authors
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*      http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*&---------------------------------------------------------------------*
*& 程序名称/PROGRAM NAME         : ZZEDIT
*& 事务代码/T-CODE               :
*& 程序描述/PROGRAM DES.         : 修改自开发程序源代码
*& 所属模块/MODULE BELONGS       :
*& 开发单位/DEVELOPMENT COMPANY  :
*& 开发作者/AUTHOR               : ABAP三叔
*& 作者邮箱/EMAIL                : 446251495@qq.com
*& 开发日期/DEVELOP DATE         : 2024.10.22
*&---------------------------------------------------------------------*
*& 使用方法：
*&    1. 可以在QAS或者PRD创建本地程序，然后将本代码粘贴激活；或者在DEV通过开发请求传输
*&    2. 支持修改SE38程序、SE37函数、SE24类方法、CMOD增强等自开发程序
*&    3. 切勿利用本程序的BUG或其它技术手段去修改SAP系统标准程序!!!
*& 免责声明
*&    1. 用户在使用本程序时应自行承担风险。用户应负责确保本程序的使用符合所有适用的法律法规，并承担因使用本程序而产生的所有后果。
*&---------------------------------------------------------------------*
*& 变更记录：                                                          *
*& DATE        DEVELOPER           REQNO       DESCRIPTIONS            *
*& ==========  =================== ==========  ========================*
*& 2024.10.22  ABAP三叔                         初始开发
*&
*&---------------------------------------------------------------------*
REPORT zzedit.
TABLES: rs38m,rs38l,seoclass,seocpdkey.

DATA gs_header TYPE header_fb.
DATA gt_result TYPE TABLE OF seop_method_w_include.
DATA gs_result TYPE seop_method_w_include.

DATA gv_error TYPE abap_bool.
DATA gv_subrc LIKE sy-subrc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t01.
  PARAMETERS: p_prog RADIOBUTTON GROUP rg1 USER-COMMAND uc1 DEFAULT 'X'.  "SE38
  PARAMETERS: p_func RADIOBUTTON GROUP rg1.                               "SE37
  PARAMETERS: p_clas RADIOBUTTON GROUP rg1.                               "SE24
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t02.
  PARAMETERS: programm TYPE rs38m-programm MODIF ID g1 MATCHCODE OBJECT sedt_programs.
  PARAMETERS: function TYPE rs38l-name MODIF ID g2 MATCHCODE OBJECT sfunc_modules.
  PARAMETERS: clsname TYPE seoclass-clsname MODIF ID g3 MATCHCODE OBJECT seo_classes_interfaces.
  PARAMETERS: cpdname TYPE seocpdkey-cpdname MODIF ID g3.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  %_p_prog_%_app_%-text = '报表'.
  %_p_func_%_app_%-text = '函数'.
  %_p_clas_%_app_%-text = '类'.
  %_programm_%_app_%-text = '报表名称'.
  %_function_%_app_%-text = '函数名称'.
  %_clsname_%_app_%-text = '类名称'.
  %_cpdname_%_app_%-text = '方法名称'.
  t01 = '功能选择'.
  t02 = '条件选择'.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE abap_true.
      WHEN p_prog.
        CASE screen-group1.
          WHEN 'G1'.
            screen-required = 2.
          WHEN 'G2' OR 'G3'.
            screen-active = 0.
        ENDCASE.
      WHEN p_func.
        CASE screen-group1.
          WHEN 'G1' OR 'G3'.
            screen-active = 0.
          WHEN 'G2'.
            screen-required = 2.
        ENDCASE.
      WHEN p_clas.
        CASE screen-group1.
          WHEN 'G1' OR 'G2'.
            screen-active = 0.
          WHEN 'G3'.
            screen-required = 2.
        ENDCASE.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR cpdname.
  PERFORM f4_cpdname.


START-OF-SELECTION.
  PERFORM check_input.
  PERFORM edit_report.


FORM check_input.
  CASE abap_true.
    WHEN p_prog.
      PERFORM check_program USING programm.
      IF gv_error = abap_true.
        MESSAGE s017(ds) WITH programm DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN p_func.
      PERFORM check_function USING function.
      IF gv_error = abap_true.
        MESSAGE s110(fl) WITH function DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN p_clas.
      PERFORM check_method.
      CASE gv_subrc.
        WHEN 1.
          MESSAGE s003(oo) WITH '类' clsname DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN 2.
          MESSAGE s003(oo) WITH '方法' cpdname DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN 3.
          MESSAGE s003(oo) WITH '类' '方法' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
      ENDCASE.
  ENDCASE.
ENDFORM.

FORM edit_report.
  DATA: BEGIN OF src OCCURS 1,
          txt(255) TYPE c,
        END OF src.

  READ REPORT programm INTO src.
  EDITOR-CALL FOR src.

  IF sy-subrc = 0.
    INSERT REPORT programm FROM src.
  ENDIF.
ENDFORM.

FORM check_program USING name TYPE rs38m-programm.
  CLEAR: function,clsname,cpdname.
  gv_error = abap_true.
  IF name IS INITIAL.
    MESSAGE '报表名称不可为空' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
*  CHECK name(1) = 'Z'.
  "检查程序是否存在
  SELECT SINGLE * INTO @DATA(ls_progdir) FROM progdir WHERE name = @name.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  "检查程序开发包
  SELECT SINGLE * INTO @DATA(ls_appl_prog) FROM appl_prog WHERE prog = @name.
  IF sy-subrc = 0.
    IF ls_appl_prog-devclass(1) <> 'Z' AND ls_appl_prog-devclass <> '$TMP'.
      RETURN.
    ENDIF.
  ELSE.
    IF name(4) = 'SAPL'.
      "函数组
      SELECT COUNT( * ) FROM tadir WHERE obj_name = @name+4(*) AND ( devclass LIKE 'Z%' OR devclass = '$TMP'  ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.
      "检查是否为类方法的包含程序
      DATA(result) = find( val = name sub = '=' ).
      IF result > 0.
        clsname = name(result).
        SELECT COUNT( * ) FROM seoclass WHERE clsname = @clsname.
        IF sy-subrc = 0.
          cl_oo_classname_service=>get_all_method_includes( EXPORTING  clsname            = clsname
                                                            RECEIVING  result             = gt_result
                                                            EXCEPTIONS class_not_existing = 1
                                                                       OTHERS             = 2 ).
          READ TABLE gt_result INTO gs_result WITH KEY incname = name.
          IF sy-subrc = 0.
            cpdname = gs_result-cpdkey-cpdname.
            PERFORM check_method.
            IF gv_error = abap_true.
              RETURN.
            ENDIF.
          ELSE.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
      "检查程序名是否存在于TADIR,如CMOD的增强程序
      SELECT SINGLE * INTO @DATA(ls_tadir) FROM tadir WHERE obj_name = @name.
      IF sy-subrc = 0.
        IF ls_tadir-devclass(1) <> 'Z' AND ls_tadir-devclass <> '$TMP'.
          RETURN.
        ENDIF.
      ELSE.
        "检查是否为SE38程序或者SE37函数的包含程序
        SELECT SINGLE * INTO @DATA(ls_d010inc) FROM d010inc
          WHERE include = @name AND
                EXISTS ( SELECT name FROM progdir WHERE name = d010inc~master ).
        IF sy-subrc = 0.
          PERFORM check_program USING ls_d010inc-master.
          IF gv_error = abap_true.
            RETURN.
          ENDIF.
        ELSE.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  gv_error = abap_false.
ENDFORM.

FORM check_function USING name.
  gv_error = abap_true.
  IF name IS INITIAL.
    MESSAGE '函数名称不可为空' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  cl_fb_function_utility=>meth_get_header_fb( EXPORTING  im_name             = CONV #( name )
                                              IMPORTING  ex_header           = gs_header
                                              EXCEPTIONS error_occured       = 1
                                                         object_not_existing = 2
                                                         OTHERS              = 3 ).
  IF sy-subrc <> 0 OR ( gs_header-devclass(1) <> 'Z' AND gs_header-devclass <> '$TMP' ).
    RETURN.
  ENDIF.
  "使用函数的包含程序名称来编辑源码
  programm = gs_header-include.
  gv_error = abap_false.
ENDFORM.

FORM check_method.
  gv_error = abap_true.
  IF clsname IS INITIAL OR cpdname IS INITIAL.
    MESSAGE '类和方法名名称不可为空' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  SELECT SINGLE * INTO @DATA(ls_tadir) FROM tadir WHERE obj_name = @clsname AND ( devclass LIKE 'Z%' OR devclass = '$TMP'  ).
  IF sy-subrc <> 0.
    gv_subrc = 3.
    RETURN.
  ENDIF.
  cl_oo_classname_service=>get_method_include( EXPORTING  mtdkey              = VALUE #( clsname = clsname cpdname = cpdname )
                                               RECEIVING  result              = programm
                                               EXCEPTIONS class_not_existing  = 1
                                                          method_not_existing = 2
                                                          OTHERS              = 3 ).
  gv_subrc = sy-subrc.
  IF sy-subrc <> 0 .
    RETURN.
  ENDIF.
  gv_error = abap_false.
ENDFORM.

FORM f4_cpdname.
  TYPES: BEGIN OF lty_cpdname,
           cpdname TYPE seocpdkey-cpdname,
         END OF lty_cpdname.
  DATA lt_cpdname TYPE TABLE OF lty_cpdname.
  DATA: lt_dynpread TYPE STANDARD TABLE OF dynpread,
        ls_dynpread TYPE dynpread.


  ls_dynpread-fieldname = 'CLSNAME'.
  APPEND ls_dynpread TO lt_dynpread.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpread
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  clsname = lt_dynpread[ 1 ]-fieldvalue.
  CHECK clsname IS NOT INITIAL.
  cl_oo_classname_service=>get_all_method_includes( EXPORTING  clsname            = clsname
                                                    RECEIVING  result             = gt_result
                                                    EXCEPTIONS class_not_existing = 1
                                                               OTHERS             = 2 ).
  IF sy-subrc <> 0.
    MESSAGE s003(oo) WITH '类' clsname DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  lt_cpdname = CORRESPONDING #( gt_result MAPPING cpdname = cpdkey-cpdname   ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CPDNAME'
      value_org       = 'S'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'CPDNAME'
    TABLES
      value_tab       = lt_cpdname
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.
