!* ===================================================================
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* ===================================================================
!* 
!* TEST CASE TITLE            : AllocateWithSourceExp07-08
!* 
!* ORIGINAL PROGRAMMER        : Dorra Bouchiha
!* PROGRAMMER                 : Izhak Jakov
!* 
!* DATE                       : June 2, 2015
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!* 
!* PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with source expression 
!* SECONDARY FUNCTIONS TESTED :
!*                              
!* 
!* DRIVER STANZA              : xlf2003
!* REQUIRED COMPILER OPTIONS  : 
!* 
!* KEYWORD(S)                 : 
!* TARGET(S)                  :
!* NUMBER OF TESTS CONDITIONS : 
!* 
!* DESCRIPTION                :
!* 
!* Defect 361745                
!* 
!* TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/SourceExp/AllocateWithSourceExp07.f
!* 
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp07
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 1

        CHARACTER(l1)  :: name  
        INTEGER(k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 2

        CLASS(Base(k2,l2)), ALLOCATABLE :: b_cmp  
        CLASS(Base(k2,l2)), ALLOCATABLE :: c_cmp 
      END TYPE Child

      TYPE(Base(4,:)), POINTER :: b1, c1
      CLASS(Base(4,:)), POINTER :: b_poly, c_poly

      ALLOCATE(b1, c1, SOURCE=Base(4,5)('Base', -99))
      CALL alloc_auto(b1)
      CALL alloc_auto(c1)
      DEALLOCATE(b1); DEALLOCATE(c1)

      ALLOCATE(b_poly, c_poly, SOURCE=Base(4,10)('Base', -99))
      CALL alloc_auto(b_poly)
      CALL alloc_auto(c_poly)

      ALLOCATE(b_poly, c_poly, SOURCE=Child(4,2,4,3)('Child', 22, Base(4,3)('Bcomp', -88) , Base(4,3)('Ccomp', -77))) 
      CALL alloc_auto(b_poly)
      CALL alloc_auto(c_poly)
      DEALLOCATE(b_poly); DEALLOCATE(c_poly)

      CONTAINS

      SUBROUTINE Alloc_auto(Arg)
      CLASS(*), INTENT(IN) :: Arg
      CLASS(*), ALLOCATABLE :: Obj

          SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                  ALLOCATE(Obj, SOURCE=Arg)

                  SELECT TYPE ( Obj )
                      CLASS IS (Base(4,*))
                        IF (ANY(Obj%my_arr .NE. -99)) ERROR STOP 10
                        IF (ANY(Obj%my_arr .NE. Arg%my_arr)) ERROR STOP 11
                        IF (Obj%name .NE. 'Base') ERROR STOP 12
                        IF (Obj%name .NE. Arg%name) ERROR STOP 13

                      CLASS DEFAULT
                         ERROR STOP 14
                  END SELECT

              CLASS IS (Child(4,*,4,*))
                  ALLOCATE(Obj, SOURCE=Arg)

                  SELECT TYPE ( Obj )
                      CLASS IS (Child(4,*,4,*))
                        IF (ANY(Obj%my_arr .NE. 22)) ERROR STOP 15
                        IF (ANY(Obj%my_arr .NE. Arg%my_arr)) ERROR STOP 16
                        IF (Obj%name .NE. 'Ch') ERROR STOP 17
                        IF (Obj%name .NE. Arg%name) ERROR STOP 18

                        IF (ANY(Obj%b_cmp%my_arr .NE. -88)) ERROR STOP 19
                        IF (ANY(Obj%b_cmp%my_arr .NE. Arg%b_cmp%my_arr)) ERROR STOP 20
                        IF (Obj%b_cmp%name .NE. 'Bco') ERROR STOP 21
                        IF (Obj%b_cmp%name .NE. Arg%b_cmp%name) ERROR STOP 22

                        IF (ANY(Obj%c_cmp%my_arr .NE. -77)) ERROR STOP 23
                        IF (ANY(Obj%c_cmp%my_arr .NE. Arg%c_cmp%my_arr)) ERROR STOP 24
                        IF (Obj%c_cmp%name .NE. 'Cco') ERROR STOP 25
                        IF (Obj%c_cmp%name .NE. Arg%c_cmp%name) ERROR STOP 26

                      CLASS DEFAULT
                         ERROR STOP 27
                  END SELECT

              CLASS DEFAULT
                 ERROR STOP 28
          END SELECT

        DEALLOCATE( Obj )

        END SUBROUTINE Alloc_auto
END PROGRAM AllocateWithSourceExp07
