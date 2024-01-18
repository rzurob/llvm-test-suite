      TYPE Base (l1)
        INTEGER, LEN  :: l1

        LOGICAL :: F0(l1) = .True.
      END TYPE

      TYPE, EXTENDS(Base) :: Child
        CLASS(*), POINTER :: Ptr
      END TYPE

      CLASS(Base(:)), POINTER :: b_poly

      ALLOCATE( Child(10) :: b_poly )

      SELECT TYPE ( s => func(b_poly) )
        CLASS IS (Child(*))
           ALLOCATE(Base(10) :: s%ptr)

        CLASS DEFAULT
           STOP 19
      END SELECT

      CONTAINS

      FUNCTION func(Arg)
        CLASS(*) :: Arg, func
        POINTER  :: func

          ALLOCATE(func, SOURCE = Arg)

      END FUNCTION
END
