MODULE Mod
      IMPLICIT NONE

      TYPE :: Base(k1, l1)
        INTEGER, KIND :: k1 = 1
        INTEGER, LEN  :: l1 = 1

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base'
      END TYPE

      TYPE,  EXTENDS(Base)  :: Child(k2, l2)
        INTEGER(k1), KIND    :: k2 = k1
        INTEGER(k1), LEN     :: l2 = k1

        CHARACTER(l2+3) :: C1 = 'Child'
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen(l3)
        INTEGER(k2), LEN     :: l3 = k2

        CHARACTER(l3)      :: C2 = CHAR(k2)
        INTEGER(k2)        :: A2 = k2
        TYPE(Base(k2, l3)) :: cmp
        CLASS(Base(k2, l3)), POINTER  :: ptr
      END TYPE

      CONTAINS

      SUBROUTINE CheckNextGen(Obj,Arg)
         CLASS(NextGen(4,*,4,*,*)) :: Obj
         TYPE (NextGen(4,*,4,*,*)) :: Arg

         print*, ASSOCIATED(Obj%ptr), ASSOCIATED(Arg%ptr)

      END SUBROUTINE

END MODULE
PROGRAM DTP_PARAMETER_04a
      USE Mod

      TYPE(NextGen(4,3,4,3,3)), PARAMETER :: n1 = NextGen(4,3,4,3,3)       &
          ( C1 = "XYZ", C2 = "ZYX", A2 = 1234, ptr = null(), cmp = Base(4,3)() )

      TYPE(NextGen(4,3,4,3,3)), TARGET :: ntgt = n1
      CLASS(Base(4,:)), POINTER :: poly

      ALLOCATE( ntgt%ptr )
      poly => ntgt
      SELECT TYPE ( poly )
          CLASS IS (NextGen(4,*,4,*,*))
            print*, ASSOCIATED(poly%ptr), ASSOCIATED(ntgt%ptr)
            CALL CheckNextGen(poly,ntgt)

          CLASS DEFAULT
             STOP 100
      END SELECT

END PROGRAM DTP_PARAMETER_04a
