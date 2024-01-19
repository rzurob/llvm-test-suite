      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base'
      END TYPE

      TYPE Container (k,l)
        INTEGER, KIND :: k
        INTEGER, LEN  :: l

        CLASS(Base(k,l)), POINTER :: ptr
      END TYPE

      TYPE(Base(l1=3)), TARGET :: btgt = Base(l1=3) ( 1, 'b1const' )
      CLASS(Container(4,:)), POINTER :: poly

      ALLOCATE( Container(4,3) :: poly )
      poly%ptr => btgt
      IF ( LEN(poly%ptr%C0)  .NE. 3 ) ERROR STOP 10

END
