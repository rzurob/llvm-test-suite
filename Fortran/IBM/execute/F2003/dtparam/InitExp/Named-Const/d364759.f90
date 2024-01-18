      TYPE :: Base(l1)
        INTEGER, LEN  :: l1 = 1

        INTEGER   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base'
      END TYPE

      TYPE, EXTENDS(Base)  :: Child(l2)
        INTEGER, LEN     :: l2 = 4

        CHARACTER(l2+3) :: C1 = 'Child'
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen(l3)
        INTEGER, LEN     :: l3 = 6

        CHARACTER(l3)            :: C2 = 'NextGen'
        TYPE(Base(l3)), POINTER  :: ptr
      END TYPE

      TYPE(NextGen(3,3,3)), PARAMETER :: n1 = NextGen(3,3,3)       &
          ( C1 = "XYZ", C2 = "ZYX", ptr = null() )

      TYPE(NextGen(3,3,3)), TARGET :: ntgt = n1
      CLASS(Base(:)), POINTER :: poly

      ALLOCATE( ntgt%ptr )
      print*, ASSOCIATED(ntgt%ptr)

      poly => ntgt
      SELECT TYPE ( poly )
          CLASS IS (NextGen(*,*,*))
             print*, ASSOCIATED(poly%ptr)

          CLASS DEFAULT
             STOP 100
      END SELECT
END
