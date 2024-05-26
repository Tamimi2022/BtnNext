&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

create widget-pool.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

//define variable hCustomer as handle no-undo .
//define variable iRow     as rowid no-undo.
//define variable iNumRows as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer Salesrep

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME Customer.CustNum Customer.Name ~
Customer.Contact Customer.Country Customer.City Customer.Address ~
Customer.PostalCode Customer.Balance Customer.EmailAddress Customer.Phone ~
Salesrep.SalesRep Salesrep.RepName Customer.Comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME Customer.CustNum ~
Customer.Name Customer.Contact Customer.Country Customer.City ~
Customer.Address Customer.PostalCode Customer.Balance Customer.EmailAddress ~
Customer.Phone Salesrep.SalesRep Salesrep.RepName Customer.Comments 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME Customer Salesrep
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Customer
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Salesrep
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH Customer NO-LOCK, ~
      EACH Salesrep OF Customer NO-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Customer NO-LOCK, ~
      EACH Salesrep OF Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Customer Salesrep
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Customer
&Scoped-define SECOND-TABLE-IN-QUERY-DEFAULT-FRAME Salesrep


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.CustNum Customer.Name ~
Customer.Contact Customer.Country Customer.City Customer.Address ~
Customer.PostalCode Customer.Balance Customer.EmailAddress Customer.Phone ~
Salesrep.SalesRep Salesrep.RepName Customer.Comments 
&Scoped-define ENABLED-TABLES Customer Salesrep
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-define SECOND-ENABLED-TABLE Salesrep
&Scoped-Define ENABLED-OBJECTS BtnFirst BtnPrev BtnNext BtnLast ~
CalcuOrdersTotal 
&Scoped-Define DISPLAYED-FIELDS Customer.CustNum Customer.Name ~
Customer.Contact Customer.Country Customer.City Customer.Address ~
Customer.PostalCode Customer.Balance Customer.EmailAddress Customer.Phone ~
Salesrep.SalesRep Salesrep.RepName Customer.Comments 
&Scoped-define DISPLAYED-TABLES Customer Salesrep
&Scoped-define FIRST-DISPLAYED-TABLE Customer
&Scoped-define SECOND-DISPLAYED-TABLE Salesrep
&Scoped-Define DISPLAYED-OBJECTS CalcuOrdersTotal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
define var C-Win as widget-handle no-undo.

/* Definitions of the field level widgets                               */
define button BtnFirst 
     label "&First" 
     size 15 by 1.14
     bgcolor 8 .

define button BtnLast 
     label "&Last" 
     size 15 by 1.14
     bgcolor 8 .

define button BtnNext 
     label "&Next" 
     size 15 by 1.14
     bgcolor 8 .

define button BtnPrev 
     label "&Prev" 
     size 15 by 1.14
     bgcolor 8 .

define variable CalcuOrdersTotal as decimal format "->>,>>9.99":U initial 0 
     label "Orders total" 
     view-as fill-in 
     size 19.6 by 1 no-undo.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query DEFAULT-FRAME for 
      Customer, 
      Salesrep scrolling.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     Customer.CustNum at row 1.71 col 16 colon-aligned widget-id 2
          view-as fill-in 
          size 7 by 1
     BtnFirst at row 1.71 col 62 widget-id 32
     Customer.Name at row 2.91 col 16 colon-aligned widget-id 4
          view-as fill-in 
          size 31 by 1
     BtnPrev at row 2.91 col 62 widget-id 34
     Customer.Contact at row 4.1 col 16 colon-aligned widget-id 6
          view-as fill-in 
          size 31 by 1
     BtnNext at row 4.1 col 62 widget-id 36
     Customer.Country at row 5.29 col 16 colon-aligned widget-id 8
          view-as fill-in 
          size 19.6 by 1
     BtnLast at row 5.29 col 62 widget-id 38
     Customer.City at row 6.48 col 16 colon-aligned widget-id 10
          view-as fill-in 
          size 19.6 by 1
     Customer.Address at row 7.67 col 16 colon-aligned widget-id 12
          view-as fill-in 
          size 32 by 1
     Customer.PostalCode at row 8.86 col 16 colon-aligned widget-id 14
          view-as fill-in 
          size 19.6 by 1
     Customer.Balance at row 10.05 col 16 colon-aligned widget-id 16
          view-as fill-in 
          size 25 by 1
     Customer.EmailAddress at row 11.24 col 16 colon-aligned widget-id 18
          view-as fill-in 
          size 32 by 1
     Customer.Phone at row 12.43 col 16 colon-aligned widget-id 20
          view-as fill-in 
          size 23 by 1
     Salesrep.SalesRep at row 13.62 col 16 colon-aligned widget-id 22
          view-as fill-in 
          size 16 by 1
     Salesrep.RepName at row 13.62 col 31 colon-aligned no-label widget-id 24
          view-as fill-in 
          size 44 by 1
     Customer.Comments at row 14.81 col 18 no-label widget-id 26
          view-as editor no-word-wrap scrollbar-horizontal scrollbar-vertical
          size 59 by 5.24
     CalcuOrdersTotal at row 20.52 col 16 colon-aligned widget-id 30
     "Comments" view-as text
          size 12 by .62 at row 15.05 col 4 widget-id 28
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1 row 1
         size 80 by 21.24
         font 6 widget-id 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
if session:display-type = "GUI":U then
  create window C-Win assign
         hidden             = yes
         title              = "<Customer Details>"
         height             = 21.24
         width              = 80
         max-height         = 21.24
         max-width          = 80
         virtual-height     = 21.24
         virtual-width      = 80
         resize             = yes
         scroll-bars        = no
         status-area        = no
         bgcolor            = ?
         fgcolor            = ?
         keep-frame-z-order = yes
         three-d            = yes
         message-area       = no
         sensitive          = yes.
else {&WINDOW-NAME} = current-window.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
assign 
       Customer.Address:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.Balance:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       CalcuOrdersTotal:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.City:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.Comments:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.Contact:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.Country:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.CustNum:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.EmailAddress:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.Name:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.Phone:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Customer.PostalCode:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Salesrep.RepName:READ-ONLY in frame DEFAULT-FRAME        = true.

assign 
       Salesrep.SalesRep:READ-ONLY in frame DEFAULT-FRAME        = true.

if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
then C-Win:hidden = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "sports2000.Customer,sports2000.Salesrep OF sports2000.Customer"
     _Options          = "NO-LOCK"
     _TblOptList       = ","
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on end-error of C-Win /* <Customer Details> */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on window-close of C-Win /* <Customer Details> */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFirst C-Win
on choose of BtnFirst in frame DEFAULT-FRAME /* First */
do:
    get first DEFAULT-FRAME.
    if available Customer then 
        display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.
    {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
    
    run setdetails.
    publish "CustomerChanged" (input Customer.CustNum) .
    // publish "getFirstCustomer" .
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLast C-Win
on choose of BtnLast in frame DEFAULT-FRAME /* Last */
do:
    get last DEFAULT-FRAME.
    if available Customer then 
        display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.
    {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
    
    run setdetails.
    publish "CustomerChanged" (input Customer.CustNum) .
    // publish "getLastCustomer" .
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext C-Win
on choose of BtnNext in frame DEFAULT-FRAME /* Next */
do:
    get next DEFAULT-FRAME.
    if not available Customer then 
        get last DEFAULT-FRAME.
    if available Customer then 
        display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.
    {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
    
     run setdetails.
    publish "CustomerChanged" (input Customer.CustNum) .
    // publish "getNextCustomer" .
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPrev C-Win
on choose of BtnPrev in frame DEFAULT-FRAME /* Prev */
do:
    get prev DEFAULT-FRAME.
    if not available Customer then 
        get first DEFAULT-FRAME.
    if available Customer then 
        display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.
    {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
    
    run setdetails.
    publish "CustomerChanged" (input Customer.CustNum) .
    // publish "getPrevCustomer" .
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
   run disable_UI.

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
  run enable_UI.
  run initialize .
  if not this-procedure:persistent then
    wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomerChanged C-Win 
procedure CustomerChanged :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input parameter piiCustNum as integer no-undo.

    define buffer bCustomer for Customer.
    define buffer bSalesrep for Salesrep.
  
    find first bCustomer where bCustomer.CustNum = piiCustNum no-lock no-error.
    find first bSalesrep where bSalesrep.SalesRep = bCustomer.SalesRep no-lock no-error.
    
    if not available bCustomer or not available bSalesRep then return.
    
    reposition {&FRAME-NAME} to rowid rowid(bcustomer), rowid(bSalesRep).
    get next {&FRAME-NAME}.
    display {&DISPLAYED-FIELDS} with frame {&frame-name}.
    run setdetails.  
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
procedure disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
  then delete widget C-Win.
  if this-procedure:persistent then delete procedure this-procedure.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
procedure enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-DEFAULT-FRAME}
  get first DEFAULT-FRAME.
  display CalcuOrdersTotal 
      with frame DEFAULT-FRAME in window C-Win.
  if available Customer then 
    display Customer.CustNum Customer.Name Customer.Contact Customer.Country 
          Customer.City Customer.Address Customer.PostalCode Customer.Balance 
          Customer.EmailAddress Customer.Phone Customer.Comments 
      with frame DEFAULT-FRAME in window C-Win.
  if available Salesrep then 
    display Salesrep.SalesRep Salesrep.RepName 
      with frame DEFAULT-FRAME in window C-Win.
  enable Customer.CustNum BtnFirst Customer.Name BtnPrev Customer.Contact 
         BtnNext Customer.Country BtnLast Customer.City Customer.Address 
         Customer.PostalCode Customer.Balance Customer.EmailAddress 
         Customer.Phone Salesrep.SalesRep Salesrep.RepName Customer.Comments 
         CalcuOrdersTotal 
      with frame DEFAULT-FRAME in window C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  view C-Win.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialize C-Win 
procedure Initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    //subscribe to "CustomerChanged" in this-procedure:instantiating-procedure .
    subscribe to "CustomerChanged" in this-procedure:instantiating-procedure.
    run setdetails.  

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrdersTotalResult C-Win 
procedure OrdersTotalResult :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
        define variable totalOrders as decimal initial 0 no-undo .
    
        if available Customer then        
        do:
            for each Order no-lock where Order.CustNum = Customer.CustNum:
                for each OrderLine no-lock where OrderLine.Ordernum = Order.Ordernum:
                    totalOrders = totalOrders + (OrderLine.Price * OrderLine.Qty - OrderLine.Discount) .
                 end .
            end .
        end .
        
        assign CalcuOrdersTotal = totalOrders .
        display CalcuOrdersTotal with frame {&frame-name} .

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetButtonsDetails C-Win 
procedure SetButtonsDetails :
/*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
        define variable iRow     as integer  no-undo.    
        define variable iNumRows as integer  no-undo.    

            assign
                iRow     = current-result-row('{&FRAME-NAME}')
                iNumRows = num-results('{&FRAME-NAME}').

            do with frame {&FRAME-NAME}:
                enable BtnFirst
                    BtnPrev
                    BtnNext
                    BtnLast
                    .

                if iRow = 1 then
                do:
                    disable BtnFirst
                        BtnPrev
                        .
                end. 
                
                /* Disable BtnNext and BtnLast if at the last row */
                else if iRow = iNumRows then
                do:
                    disable BtnLast
                        BtnNext
                        .
/*                        message "Current Row: " iRow skip         */
/*                        "Total Rows: " iNumRows view-as alert-box.*/
                        
                end.
        end .
   end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDetails C-Win
procedure SetDetails:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    run SetButtonsDetails.
    run OrdersTotalResult. 

end procedure.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



