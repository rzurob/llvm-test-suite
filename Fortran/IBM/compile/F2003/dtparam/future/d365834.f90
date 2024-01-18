MODULE Mod
      TYPE Base (k1)
        INTEGER, KIND :: k1 = 4

        INTEGER(k1) :: A01(k1) = -1 
      END TYPE

      TYPE :: Child (k2)
        INTEGER, KIND :: k2 = 4

        TYPE(Base(k2)) :: arr(2) = [(Base(k2) (77), Base(k2) (88)) ]
      END TYPE

END MODULE
END 
