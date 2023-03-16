;;; ==================================================================================================== ;;;
;;; Find Value Function ============================================================ Find Value Function ;;;

;;; --------------------------------------------------------------------------------- ;;;
;;; Created By:   Garrett Beck
;;; Date Created: 2023/01/17
;;; Source:       https://github.com/GitHubUser5376/AutoLisp_Find_Value
;;; Example:      (C:FindValue-Debugger nil "ByLayer")
;;; --------------------------------------------------------------------------------- ;;;
(defun C:FindValue-Debugger (sTag vValue / ; Input variables
    ObjPath lResult ;----------------------; Local variable
    *lVlaObjects* *vla-getlist* *lResults* ; sub function use variables
    *error* ;------------------------------; sub function use variables
    FindValue-vla-getlist ;----------------; Local sub function
    FindValue-vl-property-available ;------; Local sub function
    RecursiveFind ;------------------------; Local sub function
    ); Local variable declarations

    ;;; ------------------------------------------------------------------------- ;;;
    ;;; Local functions ----------------------------------------- Local functions ;;;

    ;; Source: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/vlax-dump-object/m-p/1079460/highlight/true#M155423
    
    ;; Returns all properties
    (defun FindValue-vla-getlist (/ fcnLambda01 fcnLambda02 fcnLambda03 lBadProperties)
        (setq fcnLambda01 (function (lambda (x) (wcmatch (strcase x) "VLA-GET-*"))))
        (setq fcnLambda02 (function (lambda (x) (substr x 9))))
        (setq fcnLambda03 (function (lambda (x) (member x lBadProperties))))
        (setq lBadProperties '("AUTOTRACKINGVECCOLOR" "COORDINATE" "DASHLENGTHAT" 
            "GRAPHICSWINLAYOUTBACKGRNDCOLOR" "GRAPHICSWINMODELBACKGRNDCOLOR" 
            "LAYOUTCROSSHAIRCOLOR" "LINETYPEAT" "MODELCROSSHAIRCOLOR" "MRUNUMBER" "OFFSETAT" 
            "SHAPENUMBERAT" "SHAPEOFFSETAT" "SHAPEROTATIONAT" "SHAPESCALEAT" "SHAPESTYLEAT" 
            "STATUSID" "TEXTAT" "TEXTWINBACKGRNDCOLOR" "TEXTWINTEXTCOLOR"
        ));setq<-quote (list)
        (setq *vla-getlist* (vl-remove-if-not fcnLambda01 (atoms-family 1)))
        (setq *vla-getlist* (mapcar fcnLambda02 *vla-getlist*))
        (setq *vla-getlist* (vl-remove-if fcnLambda03 *vla-getlist*))
    );FindValue-vla-getlist

    ;; Returns available properties
    (defun FindValue-vl-property-available (en / fcnLambda01 fcnLambda02 lBadProperties lReturn)
        (setq fcnLambda01 (function (lambda (x) (vlax-property-available-p en x))))
        (setq lReturn (vl-remove-if-not fcnLambda01 *vla-getlist*))
    );FindValue-vl-property-available

    ;;; Local functions ----------------------------------------- Local functions ;;;
    ;;; ------------------------------------------------------------------------- ;;;
    ;;; Recursive function ----------------------------------- Recursive function ;;;
    
    ;; Checks all objects for matching inputs
    (defun RecursiveFind (sTag vValue vlaObject lPath / 
        lProperties sProperty vProperty objItem lReturn iItr1 *iItr2*)

        (GB:Print "==============================================================")
        ;(GB:Print "sTag     " sTag      T nil)
        ;(GB:Print "vValue   " vValue    T nil)
        (GB:Print "vlaObject" vlaObject T nil)
        (GB:Print "lPath    " (reverse lPath) T nil)
        (GB:Print "iItr1" (setq *iItr1*  (1+ *iItr1* )) T)
        (GB:Print "Level" (setq *iLevel* (1+ *iLevel*)) T)

        ;; Adding object to the list
        (setq *lVlaObjects* (cons vlaObject *lVlaObjects*))

        ;; Collecting properties
        (setq lProperties (FindValue-vl-property-available vlaObject))
        (GB:Print "lProperties" lProperties T nil)

        ;; Each property within the object
        (setq lReturn (list))
        (foreach sProperty lProperties
            (GB:Print "sProperty" sProperty T)
            (setq vProperty (vl-catch-all-apply 'vlax-get (list vlaObject sProperty)))
            (GB:Print "vProperty" vProperty T)
            (GB:Print "")
            ;; Recording matching properties
            (cond 
                ;; Condition 1
                ((and sTag vValue)
                    (if (and (= vValue vProperty)(= sTag sProperty))
                        (setq lReturn (cons (list (reverse lPath) sProperty vProperty) lReturn))
                    );if
                ); Condition 1
                ;; Condition 2
                (sTag
                    (if (= sTag sProperty)
                        (setq lReturn (cons (list (reverse lPath) sProperty vProperty) lReturn))
                    );if
                ); Condition 2
                ;; Condition 3
                (vValue
                    (if (= vValue vProperty)
                        (setq lReturn (cons (list (reverse lPath) sProperty vProperty) lReturn))
                    );if
                ); Condition 3
            );cond
            
            ;; Object Recursion
            (if (and (= (type vProperty) 'VLA-OBJECT)(not (member vProperty *lVlaObjects*)))
                (setq lReturn (append lReturn 
                    (RecursiveFind sTag vValue vProperty (cons sProperty lPath))
                ));setq<-append
            );if

            ;; List Recursion
            (if (and (= sProperty "COUNT")(= (type vProperty) 'INT))(progn
                (setq iItr1 0)
                (vlax-for objItem vlaObject
                    (if (and (not (member objItem *lVlaObjects*))(= (type objItem) 'VLA-OBJECT))
                        (setq lReturn (append lReturn 
                            (RecursiveFind sTag vValue objItem (cons (strcat "ITEM " (itoa iItr1)) lPath))
                        ));setq<-append
                    );if
                    (setq iItr1 (1+ iItr1))
                );vlax-for
            ));if<-progn
        );foreach

        (GB:Print "Level-End" *iLevel* T)
        (GB:Print "--------------------------------------------------------------")
        (setq *iLevel* (1- *iLevel*))

        lReturn
    );RecursiveFind

    ;;; Recursive function ----------------------------------- Recursive function ;;;
    ;;; ------------------------------------------------------------------------- ;;;
    ;;; Function call --------------------------------------------- Function call ;;;
    
    ;; Starting recursive search - Returns list variable
    (setq *iItr1* 0)
    (setq *iLevel* 0)
    (FindValue-vla-getlist)
    (if (= sTag "")(setq sTag nil))
    (GB:Print-Start nil T T)
    (setq *lResults* (RecursiveFind sTag vValue (vlax-get-acad-object) nil))
    (GB:Print-End)
    (princ "\n*lResults* : ")(prin1 *lResults*)(terpri)

    ;; Printing results
    (foreach lResult *lResults* 
        (princ "\n---------------------------------------\n")
        (foreach ObjPath (nth 0 lResult)(princ "\nObject : ")(prin1 ObjPath)(terpri))
        (princ "\nTag   : ")(prin1 (nth 1 lResult))(terpri)
        (princ "\nValue : ")(prin1 (nth 2 lResult))(terpri)
    );foreach
    (princ)
    *iItr1*
);FindValue

;;; Find Value Function ============================================================ Find Value Function ;;;
;;; ==================================================================================================== ;;;
;;; Debug Printer ======================================================================== Debug Printer ;;;

;;; ------------------------------------------------------------------------- ;;;
;;; Date created: 2023/01/17
;;; Created By: Garrett Beck
;;; Source: https://github.com/GitHubUser5376/AutoLisp_Debugger/new/main
;;; ------------------------------------------------------------------------- ;;;

;; Global variables
(setq *gbPrint:IsList* nil)
(setq *gbPrint:bPrintScreen* nil)
(setq *gbPrint:bPrintFile* nil)
(setq *gbPrint:bClearOldLog* nil)

;; Debug Printer
(defun GB:Print (sMsg vValue bValue bType / sString FileID)

    ;; Error catch
    (if (/= 'STR (type sMsg))(progn 
        (GB:Print "Error : An invalid variable type was entered into GB:Print's \"sMsg\".")
        (GB:Print "Error. Invalid variable type" sMsg nil T)
        (exit)
    ));if<-progn

    ;; Print to file - Clear / Append
    (cond 
        (   *gbPrint:bClearOldLog*
            (setq FileID (open (strcat (getenv "LOCALAPPDATA") "\\Temp\\AutoLisp-Print.txt") "w"))
            (setq *gbPrint:bClearOldLog* nil)
        ); Condition 1
        ;; Condition 2
        (   *gbPrint:bPrintFile*
            (setq FileID (open (strcat (getenv "LOCALAPPDATA") "\\Temp\\AutoLisp-Print.txt") "a"))
        ); Condition 2
    );if

    ;; Extra space - Output file only
    (cond 
        ;; Condition 1 - Adds a new line after the list value, before the next value type is added.
        (   (and *gbPrint:bPrintFile* (/= 'LIST (type vValue)) *gbPrint:IsList*)
            (write-line "" FileID)
            (setq *gbPrint:IsList* nil)
        ); Condition 1
        ;; Condition 2 - Adds a new line before the list's value is added.
        (   (and *gbPrint:bPrintFile* (= 'LIST (type vValue)))
            (write-line "" FileID)
            (setq *gbPrint:IsList* T)
        ); Condition 2
    );cond

    ;; Outbound message
    (setq sString (cond 
        (bType (strcat sMsg " : " (vl-prin1-to-string (type vValue))));-----; Condition 1
        ((or bValue vValue) (strcat sMsg " : " (vl-prin1-to-string vValue))); Condition 2
        (T sMsg); Else
    ));setq<-cond

    ;; Writing to output file
    (if *gbPrint:bPrintFile* (write-line sString FileID))

    ;; Printing to the screen
    (if *gbPrint:bPrintScreen* (princ sString))
    (setq FileID (close FileID))
    
    ;; Return value
    vValue
);GB:Print

;; Ending debug printer
(defun GB:Print-End (/)
    (setq *gbPrint:bPrintScreen* nil)
    (setq *gbPrint:bPrintFile*   nil)
    (setq *gbPrint:bClearOldLog* nil)
    (princ)
);GB:Print-End

;; Starting debug printer
(defun GB:Print-Start (bPrintScreen bPrintFile bClearOldLog /)
    (setq *gbPrint:bPrintScreen* (if bPrintScreen T nil))
    (setq *gbPrint:bPrintFile*   (if bPrintFile   T nil))
    (setq *gbPrint:bClearOldLog* (if bClearOldLog T nil))
    (princ)
);GB:Print-Start

;;; Debug Printer ======================================================================== Debug Printer ;;;
;;; ==================================================================================================== ;;;
