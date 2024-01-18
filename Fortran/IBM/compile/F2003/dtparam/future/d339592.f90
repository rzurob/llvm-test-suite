MODULE Mod
      TYPE Base (k1)
        INTEGER, KIND :: k1 = 4

        INTEGER(k1) :: A01(k1) = -1
      END TYPE

      TYPE :: Child (k2)
        INTEGER, KIND :: k2 = 4

        TYPE(Base(k2)) :: arr(1) = [Base(k2) ( )]
      END TYPE
END MODULE
Use Mod 
END
