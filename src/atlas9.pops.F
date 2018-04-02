      subroutine pops (code, mode, number)
      implicit none
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.ifblkk'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.tempbl'
      include 'common.xabund'
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  code, number(maxd, *)
      integer  mode
*
*------------------------------- EXTERNALS ----------------------------
*
      external molec, nelect, nmolec, pfsaha
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      integer  j, itemp1, ion, iz, nion, nnnn
      save     itemp1
*
*--------------------------- INITIALIZATION ---------------------------
*
      data itemp1 / 0 /
*
*------------------------------- EXECUTION -----------------------------
*
      if (ifmol) then
!         if (ifpres .and. (itemp .ne. itemp1)) call nmolec ((mode))
         if (itemp .ne. itemp1) call nmolec ((mode))
         itemp1 = itemp
         if (code .gt. 0.0d0) call molec((code), (mode), number)
*
      else
!         if (ifpres .and. (itemp .ne. itemp1)) call nelect
         if (itemp .ne. itemp1) call nelect
         itemp1 = itemp
*
         if (code .gt. 0.0d0) then
*
            if (code .lt. 100.0d0) then
               iz   = code
               nion = (code - dble(iz)) * 100.0d0 + 1.5
*
               do j = 1, nrhox
                  call pfsaha((j), (iz), (nion), (mode), number)
*
*.... PFSAHA RETURNS IONIZATION FRACTIONS OR  
*....                IONIZATION FRACTIONS / PARTITION FUNCTIONS 
*.... SO CONVERT TO NUMBER DENSITIES
*
                  nnnn = nion
                  if ( mode .lt. 10) nnnn = 1
*
                  do ion = 1, nnnn
                     number(j, ion) = number(j, ion) * xnatom(j) *
     &                                xabund(iz)
                  end do
*
               end do
*
            else
               write (16, *) '1molecules off'
	       print *, '1molecules off'
               stop
            end if
*
         end if
*
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   P O P S ****************
*
      subroutine molec (codout, mode, number)
      implicit none
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.elembl'
      include 'common.ifblkk'
      include 'common.ifequa'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.tempbl'
      include 'common.xabund'
      include 'common.xnmolb'
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  codout, number(maxd, *)
      integer           mode
*
*------------------------------- EXTERNALS ----------------------------
*
      external pfsaha
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  c
      integer           i, id, ion, j, jmol, nn
      logical           iread, match, sec400
      save              iread
*
*--------------------------- INITIALIZATION ---------------------------
*
      data iread / .false. /
*
*------------------------------- EXECUTION -----------------------------
*
!      if ((.not. iread) .and. (.not. ifpres)) then
*
!         read (15, 151) nummol
!151      format (i5)
*
!         do jmol = 1, nummol
!            read (15, 152) code(jmol)
!152         format (f20.2)
!            read (15, 153) (xnmol(j, jmol), j = 1, nrhox)
!153         format (1p8d10.3)
!            write (16, 154) ' xnmol for code', code(jmol), 
!     &                      (xnmol(j, jmol), j=1,nrhox)
!154         format (a, f20.2 / (1p8d10.3))
!         end do
*
!         read  (15, 158)
!         read  (15, 158) (xnatom(j), j = 1, nrhox)
!         read  (15, 158)
!         read  (15, 158) (rho(j), j = 1, nrhox)
!         read  (15, 158)
!         read  (15, 158) (xne(j), j = 1, nrhox)
!158      format (1p8d10.3)
!         write (16, 159) ' xnatom', (xnatom(j), j = 1, nrhox)
!         write (16, 159) ' rho', (rho(j), j = 1, nrhox)
!         write (16, 159) ' xne', (xne(j), j = 1, nrhox)
!159      format (a / (1p8d10.3))
!         iread = .true.
!      end if
*
      if (codout .ge. 100.0d0) then
         jmol = 1
*
         do while (jmol .le. nummol .and. code(jmol) .ne. codout)
            jmol = jmol + 1
         end do
*
         if (jmol .gt. nummol) then
            write (16, 202) ' WARNING from subroutine molec:',
     &                      ' no data for molecule', codout,
     &                      ' - setting its number densities to 0.0'
202         format (a / 5x, a, f20.2 / 5x, a)
*
            do j = 1, nrhox
               number(j, 1) = 0.0d0
            end do
*
         else
*
            do j = 1, nrhox
               number(j, 1) = xnmol(j, jmol)
            end do
*
         end if
*
      else
         c  = codout
*
         if (mode .eq. 11) then
            nn = (c - dble( int( c ))) * 100.0d0 + 1.5
*
         else
            nn = 1
         end if
*
         i = 0
         sec400 = .false.
*
         do while (i .lt. nn .and. .not. sec400)
            i = i + 1
            ion = nn - i + 1
            jmol = 1
            match = .false.
*
            do while (.not. match .and. jmol .le. nummol)
*
               if (abs(code(jmol) - c) .lt. 0.001d0) then
                  match = .true.
*
               else
                  jmol = jmol + 1
               end if
*
            end do
*
            if (match) then
*
               do j = 1, nrhox
                  number(j, ion) = xnmol(j, jmol)
               end do
*
            else
               id    = codout
               jmol  = 1
               match = .false.
*
               do while (.not. match .and. jmol .le. nummol)
*
                  if(int(code(jmol)) .eq. id) then
                     match = .true.
*
                  else
                     jmol = jmol + 1
                  end if
*
               end do
*
               if (match) then
*
                  do j = 1, nrhox
                     number(j, ion) = 0.0d0
                  end do
*
               else
                  sec400 = .true.
*
               end if
*
            end if
            if(.not. sec400) c = c - 0.01d0
         end do
*
         if(sec400) then
            ion = (codout - dble(id)) * 100.0d0 + 1.5d0
*
            if(mode .eq. 1) then
               nn = 1
*
            else
               nn  = ion
            end if
*
            do j = 1, nrhox
               call pfsaha ((j), (id), (ion), (mode), number)
*
               do i = 1, nn
                  number(j, i) = number(j, i) * xnatom(j) * xabund(id)
               end do
*
            end do
*
         end if
*
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   M O L E C **************
*
      subroutine nelect
      implicit none
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.constb'
*
      include 'common.elembl'
      include 'common.edenbl'
      include 'common.iterbl'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.tempbl'
      include 'common.xabund'
*
*------------------------------ CONSTANTS ----------------------------
*
      double precision  factre, factrm, factrp
      integer           c4, c5, c12, nz
      parameter (c4 = 4, c5 = 5, c12 = 12, nz = 10, 
     &           factre = 1.0d0 / 0.999d0,
     &           factrm = 0.999d0 / 1.001d0,
     &           factrp = 1.001d0)
*
*------------------------------- EXTERNALS ----------------------------
*
      external pfsaha
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      character         ee(nz)*2
      double precision  e(nz), eion(7), elec(maxd), error, 
     &                  pfmin(13), pfplus(13), x(nz), x1, xnenew, 
     &                  xntot, xnumb(maxd, 3)
      integer           i, ion, iz, j, l, nelem, nelemz(nz), nion,
     &                  nionz(nz)
      logical           mask(nz), more
      save              nelemz, nionz
*
*--------------------------- INITIALIZATION ---------------------------
*
      data nelemz / 1, 2, 6, 11, 12, 13, 14, 19, 20, 26 /
      data nionz  / 2, 3, 2,  2,  2,  2,  2,  2,  2,  2 /
*
*------------------------------- EXECUTION -----------------------------
*
      if(.not. ifedns) then
*
         do i = 1, nz
            nelem = nelemz(i)
            ee(i) = elem(nelem)
         end do
*
         write (16, 402) 'electron contributions',
     &                  (ee(i), i = 1, nz), (ee(i), i = 1, nz)
402      format ( // 50x, a // 2x, 20(4x, a2) / )

          xne(1) = 0.5d0 * p(1) / tk(1)
      end if
*
      do j = 1, nrhox
         if (j .gt. 1 .and. .not. ifedns) xne(j) = xne(j-1) * 
     &      p(j) / p(j-1)
         xntot     = p(j) / tk(j)
         xnatom(j) = xntot - xne(j)
*
         do i = 1, nz
            mask(i) = .true.
         end do
*
         l = 0
         more = .true.
*
         do while (more .and. l .lt. 200)
            l = l + 1
            xnenew = 0.0d0
*
            do i = 1, nz
*
               if (mask(i)) then
                  iz     = nelemz(i)
                  nion   = nionz(i)
                  call pfsaha ((j), (iz), (nion), c4, elec)
                  e(i)   = elec(j)
                  x(i)   = elec(j) * xnatom(j) * xabund(iz)
                  xnenew = xnenew + x(i)
               end if
*
            end do
*
            xnenew    = 0.5d0 * (xnenew + xne(j))
            error     = abs(xne(j) - xnenew) / xnenew

            xne(j)    = xnenew
            xnatom(j) = xntot - xne(j)
*
            if (error .lt. 5.0d-4) then
               more = .false.
*
            else
*
               if (j .gt. 1) then
                  x1 = 1.0d-5 * xne(j)
                  if (error .lt. 0.05d0) x1 = x1 * 10.0d0
*
                  do i = 1, nz
                     if (x(i) .lt. x1) mask(i) = .false.
                  end do
*
               end if
*
            end if
*
         end do
*
         if (l .ge. 200) then
            write (16, 250) ' xne does not converge for depth', j
	print *, ' xne does not converge for depth', j
	print *, ' error is ', error
250         format (a, i3)
            stop
*
         else
            rho(j) = xnatom(j) * wtmole * amu
*
            if (ifedns) then
               edens(j) = 1.5d0 * xntot * tk(j)
*
               do i = 1, nz
                  iz      = nelemz(i)
                  nion    = nionz(i)
*
                  t(j)    = t(j)    * factrp
                  tk(j)   = tk(j)   * factrp
                  tkev(j) = tkev(j) * factrp
                  call pfsaha((j), (iz), (nion), c5, pfplus)
*
*.... THE FOLLOWING LOOP IS NEEDED TO AVOID AN EQUIVALENCE IN THE
*.... ORIGINAL.  THE VALUES FOR EION ARE RETURNED FROM PFSAHA IN
*.... THE VARIABLE PFPLUS(7+).
*
                  do ion = 1, nion
                     eion(ion) = pfplus(ion + 6)
                  end do
*
                  t(j)    = t(j)    * factrm
                  tk(j)   = tk(j)   * factrm
                  tkev(j) = tkev(j) * factrm
                  call pfsaha((j), (iz), (nion), c5, pfmin)
*
                  t(j)    = t(j)    * factre
                  tk(j)   = tk(j)   * factre
                  tkev(j) = tkev(j) * factre
                  call pfsaha((j), (iz), (nion), c12, xnumb)
*
                  do ion = 1, nion
                     xnumb(j, ion) = xnumb(j, ion) * xnatom(j) * 
     &                               xabund(iz)
                     edens(j) = edens(j) + xnumb(j, ion) * tk(j) * 
     &                          (eion(ion) / tkev(j) + 
     &                          (pfplus(ion) - pfmin(ion)) / 
     &                          (pfplus(ion) + pfmin(ion)) * 1000.0d0)
                  end do
*
               end do
*
               edens(j) = edens(j) / rho(j)
*
            end if 
*
            do i = 1, nz
               x(i) = x(i) / xne(j)
            end do
*
            write (16, 404) j, (x(i), i = 1, nz), (e(i), i = 1, nz)
404         format (i3, 20f6.3)
*
         end if
*
      end do
*
      end
*
*********** E N D   O F   S U B R O U T I N E   N E L E C T ************
*
      subroutine nmolec (mode)
      implicit none
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      integer mode
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.edenbl'
      include 'common.elembl'
      include 'common.ifequa'
      include 'common.iterbl'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.tempbl'
      include 'common.xabund'
      include 'common.xnmolb'
*
*------------------------------ CONSTANTS ----------------------------
*
      double precision  factre, factrm, factrp
      integer           c3, c5, c12
      parameter (c3 = 3, c5 = 5, c12 = 12,
     &           factre = 1.0d0 / 0.999d0,
     &           factrm = 0.999d0 / 1.001d0,
     &           factrp = 1.001d0 )
*
*------------------------------- EXTERNALS ----------------------------
*
      external pfsaha, solvit
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  amass, d, deq(maxmeq, maxmeq), eion(7), 
     &                  eq(maxmeq), eqold(maxmeq), equilj(maxmol), 
     &                  frac(maxd, 6), pfm(13), pfmin(maxd), pfp(13), 
     &                  pfplus(maxd), ratio, scale, term, tminus, 
     &                  tplus, x, xab(maxmeq), xn(maxmeq), xn100, 
     &                  xneq, xnsave(maxd, maxmeq), xntot, 
     &                  xnz(maxd, maxmeq)
      integer           id, ion, j, jmol, jmol1, jmol10, k,  
     &                  l, locj1, locj2, locm, lock, m, ncomp, nequa1
      logical           iferr, klimit

      save xnsave
*
*------------------------------- EXECUTION -----------------------------
*
      nequa1 = nequa + 1
*
      do k = 2, nequa
         id = idequa(k)
         if(id .lt. 100) xab(k) = max(xabund(id), 1.0d-20)
      end do
*
      if(id .eq. 100) xab(nequa) = 0.0d0
      xntot = p(1) / tk(1)
      xn(1) = xntot * 0.5d0
      if(t(1) .lt. 4000.0d0) xn(1) = xntot
      x = xn(1) * 0.1d0
*
      do k = 2, nequa
         xn(k) = x * xab(k)
      end do
*
      if(id .eq. 100) xn(nequa) = x
      xne(1) = x
*
      do j = 1, nrhox
         xntot = p(j) / tk(j)
*
         if(j .gt. 1) then
            ratio  = p(j) / p(j-1)
            xne(j) = xne(j-1) * ratio
*
            do k = 1, nequa
               xn(k) = xn(k) * ratio
            end do
*
         end if
*
         if(ifedns) then
*
            do k = 1, nequa
               xn(k) = xnsave(j, k)
            end do
*
         end if
*
         do jmol = 1, nummol
            ncomp = locj(jmol+1) - locj(jmol)
*
            if(equil(1, jmol) .ne. 0.0d0) then
               ion = (code(jmol) - dble(int(code(jmol)))) * 
     &               100.0d0 + 0.5d0
               equilj(jmol) = 0.0d0
               if(t(j) .le. 10000.0d0) equilj(jmol) = 
     &            exp(equil(1, jmol) / tkev(j) - 
     &            equil(2, jmol) + (equil(3, jmol) + 
     &            (-equil(4, jmol) + (equil(5, jmol) - equil(6, jmol) *
     &            t(j)) * t(j)) * t(j)) * t(j) - 1.5d0 * 
     &            (dble(ncomp - ion - ion - 1)) * tlog(j))
*
            else 
*
               if(ncomp .le. 1) then
                  equilj(jmol) = 1.0
*
               else
                  id  = code(jmol)
                  ion = ncomp-1
                  call pfsaha((j), (id), (ncomp), c12, frac)
                  equilj(jmol) = frac(j, ncomp) / frac(j, 1) * 
     &                           xne(j) ** ion
               end if
*
            end if
*
         end do
*
         do k = 1, nequa
            eqold(k) = 0.0d0
         end do
*
*.... SET UP 1ST ORDER EQUATIONS FOR THE CHANGE IN NUMBER DENSITY OF
*.... EACH ELEMENT
*
         iferr = .true.
*
         do while (iferr)
*
            do l = 1, nequa
*
               do k = 1, nequa
                  deq(k,l) = 0.0d0
               end do
*
            end do
*
            eq(1) = -xntot
*
            do k = 2, nequa
               deq(1, k) = 1.0d0
               deq(k, k) = 1.0d0
               deq(k, 1) = -xab(k)
               eq(1)     = eq(1) + xn(k)
               eq(k)     = xn(k) - xab(k) * xn(1)
            end do
*
            if (idequa(nequa) .ge. 100) then
               deq(nequa, nequa) = -1.0d0
               eq(nequa) = -xn(nequa)
            end if
*
            do jmol = 1, nummol
               ncomp = locj(jmol + 1) - locj(jmol)
*
               if (ncomp .ne. 1) then
                  term  = equilj(jmol)
                  locj1 = locj(jmol)
                  locj2 = locj(jmol+1) - 1
*
                  do lock = locj1, locj2
                     k = kcomps(lock)
*
                     if (k .eq. nequa1) then
                        term = term / xn(nequa)
*
                     else
                        term = term * xn(k)
                     end if
*
                  end do
*
                  eq(1) = eq(1) + term
*
                  do lock = locj1, locj2
                     k = kcomps(lock)
*
                     if (k .lt. nequa1) then
                        d = term / xn(k)
*
                     else
                        k = nequa
                        d = -term / xn(k)
                     end if
*
                     eq(k)     = eq(k) + term
                     deq(1, k) = deq(1, k) + d
*
                     do locm = locj1, locj2
                        m = kcomps(locm)
                        if (m .eq. nequa1) m = nequa
                        deq(m, k) = deq(m, k) + d
                     end do
*
                  end do
*
*.... CORRECTION TO CHARGE EQUATION FOR NEGATIVE IONS
*
                  k = kcomps(locj2)
*
                  if (idequa(k) .eq. 100) then
*
                     do lock = locj1, locj2
                        k = kcomps(lock)
                        d = term / xn(k)
                        if (k .eq. nequa) eq(k) = eq(k) - term - term
*
                        do locm = locj1, locj2
                           m = kcomps(locm)
                           if (m .eq. nequa) deq(m,k) = deq(m,k)-d-d
                        end do
*
                     end do
*
                  end if
*
               end if
*
            end do
*
            call solvit (deq, (maxmeq), (nequa), eq)
            iferr = .false.
            scale = 100.0d0
*
            do k = 1, nequa
               if (abs (eq(k) / xn(k)) .gt. 0.001d0) iferr = .true.
               if (eqold(k) * eq(k) .lt. 0.0d0) eq(k) = eq(k) * 0.69d0
               xneq = xn(k) - eq(k)
               xn100 = xn(k) * 0.01d0
*
               if (xneq .lt. xn100) then
                  xn(k) = xn(k) / scale
                  if (eqold(k) * eq(k) .lt. 0.0d0) scale = sqrt(scale)
*
               else
                  xn100 = xn(k) * 100.0d0
                  xn(k) = xneq
               end if
*
               eqold(k) = eq(k)
            end do
*
         end do
*
         do k = 1, nequa
            xnz(j, k) = xn(k)
         end do
*
         xnatom(j) = xn(1)
         rho(j)    = xnatom(j) * wtmole * 1.660d-24
         if (idequa(nequa) .eq. 100) xne(j) = xn(nequa)
*
         do jmol = 1, nummol
            ncomp = locj (jmol + 1) - locj(jmol)
            xnmol(j, jmol) = equilj(jmol)
            locj1 = locj(jmol)
            locj2 = locj (jmol + 1) - 1
*
            do lock = locj1, locj2
               k = kcomps(lock)
*
               if (k .eq. nequa1) then
                  xnmol(j, jmol) = xnmol(j, jmol) / xn(nequa)
*
               else
                  xnmol(j, jmol) = xnmol(j, jmol) * xn(k)
               end if
*
            end do
*
         end do
*
      end do
*
      if(ifedns) then
*
         do j = 1, nrhox
            edens(j) = 1.5d0 * p(j)
         end do
*
         do jmol = 1, nummol
            ncomp = locj(jmol+1) - locj(jmol)
*
            if(equil(1,jmol) .eq. 0.0d0) then
               id = code(jmol)
*
               do j = 1, nrhox
                  t(j)    = t(j)    * factrp
                  tk(j)   = tk(j)   * factrp
                  tkev(j) = tkev(j) * factrp
                  call pfsaha((j), (id), (ncomp), c5, pfp)
*
*.... THE FOLLOWING LOOP IS NEEDED TO AVOID AN EQUIVALENCE IN THE
*.... ORIGINAL.  THE VALUES FOR EION ARE RETURNED FROM PFSAHA IN
*.... THE VARIABLE PFP(7+).
*
                  do ion = 1, 7
                     eion(ion) = pfp(ion + 6)
                  end do
*
                  t(j)    = t(j)    * factrm 
                  tk(j)   = tk(j)   * factrm
                  tkev(j) = tkev(j) * factrm
                  call pfsaha((j), (id), (ncomp), c5, pfm)
*
                  t(j)    = t(j)    * factre 
                  tk(j)   = tk(j)   * factre
                  tkev(j) = tkev(j) * factre 
                  ion = ncomp
                  edens(j) = edens(j) + xnmol(j, jmol) * tk(j) *
     &               (eion(ion) / tkev(j) + (pfp(ion) - pfm(ion)) / 
     &               (pfp(ion) + pfp(ion)) * 1000.0d0)
               end do
*
            else
*
               do j = 1, nrhox
                  tplus     = t(j) * factrp
                  tminus    = t(j) * 0.999d0
                  pfplus(j) = exp(-equil(2,jmol) + (equil(3,jmol) + 
     &               (-equil(4,jmol) + (equil(5,jmol) - equil(6,jmol) *
     &               tplus) * tplus) * tplus) * tplus) + 1.0d-30
                  pfmin(j)  = exp(-equil(2,jmol) + (equil(3,jmol) + 
     &               (-equil(4,jmol) + (equil(5,jmol) - equil(6,jmol) *
     &               tminus) * tminus) * tminus) * tminus) + 1.0d-30
               end do
*
               locj1  = locj(jmol)
               locj2  = locj(jmol+1) - 1
               lock   = locj1 - 1
               klimit = .false.
*
               do while(lock .lt. locj2 .and. .not. klimit)
                  lock = lock + 1
                  k    = kcomps(lock)
*
                  if(k .gt. nequa) then
                     klimit = .true.
*
                  else
                     id = idequa(k)
*
                     do j = 1, nrhox
                        t(j)    = t(j)    * factrp
                        tk(j)   = tk(j)   * factrp
                        tkev(j) = tkev(j) * factrp
                        call pfsaha((j), (id), 1, c3, frac)
                        pfplus(j) = pfplus(j) * frac(j, 1)
*
                        t(j)    = t(j)    * factrm 
                        tk(j)   = tk(j)   * factrm 
                        tkev(j) = tkev(j) * factrm 
                        call pfsaha((j),(id), 1, c3, frac)
                        pfmin(j) = pfmin(j) * frac(j, 1)
*
                        t(j)    = t(j)    * factre
                        tk(j)   = tk(j)   * factre 
                        tkev(j) = tkev(j) * factre 
                     end do
*
                  end if
*
               end do
*
               if (.not. klimit) then
*
                  do j = 1, nrhox
                     edens(j) = edens(j) + xnmol(j, jmol) * tk(j) *
     &                  (-equil(1, jmol) / tkev(j) + (pfplus(j) - 
     &                   pfmin(j)) / (pfplus(j) + pfmin(j)) * 1000.0d0)
                  end do
*
               end if
*
            end if
*
         end do
*
         do j = 1, nrhox
            edens(j) = edens(j) / rho(j)
         end do
*
      else
*
         do k = 1, nequa
*
            do j = 1, nrhox
               xnsave(j, k) = xnz(j, k)
            end do
*
         end do
*
         write (16, 112) 'rhox', 't', 'p', 'xne', 'xnatom', 'rho',
     &       (j, rhox(j), t(j), p(j), xne(j), xnatom(j), rho(j), 
     &        j = 1, nrhox )
112      format ( /////
     &           11x, a, 9x, a, 11x, a, 10x, a, 8x, a, 8x, a /
     &           (i5, 1p6e12.3) )
         jmol1 = 1
*
         do while (jmol1 .le. nummol)
            write (16, 113) 'molecular number densities'
113         format ( /// 35x, a /)
            jmol10 = jmol1
            id     = code(jmol1)
*
            do while (int(code(jmol10 + 1)) .eq. id)
               jmol10 = jmol10 + 1
            end do
*
            write (16, 114) (code(jmol), jmol = jmol1, jmol10)
114         format (5x, 6f12.2)
*
            do j = 1, nrhox
               write(16, 115) j, 
     &                       (xnmol(j, jmol), jmol = jmol1, jmol10)
115            format ( i5, 1p6e12.3)
            end do
*
            jmol1 = jmol10 + 1
         end do
*
*
         if (mode .ne. 2 .and. mode .ne. 12) then
*
            do k = 2, nequa
               id = idequa(k)
*
               if (id .eq. 100) then
*
                  do j = 1, nrhox
                     xnz(j, k) = xnz(j, k) * 0.5d0 / 2.4148d15 / t(j) /
     &                           sqrt(t(j) )
                  end do
*
               else
*
*.... CALCULATE PARTITION FUNCTIONS
*
                  do j = 1, nrhox
                     call pfsaha ((j), (id), 1, c3, frac)
                     xnz(j, k) = xnz(j, k) / frac(j, 1) / 1.8786d20 /
     &                           sqrt((atmass(id) * t(j)) ** 3)
                  end do
*
               end if
*
            end do
*
            do jmol = 1, nummol
               ncomp = locj(jmol + 1) - locj(jmol)
*
               if (equil(1, jmol) .eq. 0.0d0) then
                  id = code(jmol)
*
                  do j = 1, nrhox
                     call pfsaha ((j), (id), (ncomp), c3, frac)
                     xnmol(j, jmol) = xnmol(j, jmol) / frac(j, 1)
                  end do
*
               else
*
                  do j = 1, nrhox
                     xnmol(j, jmol) = exp(equil(1, jmol) / tkev(j))
                  end do
*
                  amass = 0.0d0
                  locj1 = locj(jmol)
                  locj2 = locj(jmol + 1) - 1
*
                  do lock = locj1, locj2
                     k = kcomps(lock)
*
                     if (k .eq. nequa1) then
*
                        do j = 1, nrhox
                           xnmol(j, jmol) = xnmol(j, jmol) /
     &                                      xnz(j,nequa)
                        end do
*
                     else
                        id = idequa(k)
                        if (id .lt. 100) amass = amass + atmass(id)
*
                        do j = 1, nrhox
                           xnmol(j, jmol) = xnmol(j, jmol) * xnz(j, k)
                        end do
*
                     end if
*
                  end do
*
                  do j = 1, nrhox
                     xnmol(j, jmol) = xnmol(j, jmol) * 1.8786d20 *
     &                                sqrt((amass * t(j)) ** 3)
                  end do
*
               end if
*
            end do
*
         end if
*
         if (ifpnch .eq. 5) then
            write (16, 150) 'number densities / partition functions'
150         format ( / 20x, a)
            write (16, 151) nummol, ' molecules'
            write (17, 151) nummol, ' molecules'
151         format (i5, a )
*
            do jmol = 1, nummol
               write (16, 152) code(jmol), (xnmol(j, jmol), j=1, nrhox)
               write (17, 152) code(jmol), (xnmol(j, jmol), j=1, nrhox)
152            format (f20.2 / (1p8e10.3))
            end do
*
            write (16, 158) ' xnatom', (xnatom(j), j = 1, nrhox)
            write (16, 158) ' rho',    (rho(j),    j = 1, nrhox)
            write (16, 158) ' xne',    (xne(j),    j = 1, nrhox)
*
            write (17, 158) ' xnatom', (xnatom(j), j = 1, nrhox)
            write (17, 158) ' rho',    (rho(j),    j = 1, nrhox)
            write (17, 158) ' xne',    (xne(j),    j = 1, nrhox)
158         format (a / (1p8e10.3))
         end if
*
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   N M O L E C ************
*
      subroutine pfsaha (j, iz, nion, mode, answer)
      implicit none
*
*.... 1988 MAR - TO AVOID OVER/UNDERFLOW AT LOW DENSITY
*.... 1993 JAN - TO USE THE SAME CUTOFF AS BOB TO GET THE SAME RESULT
*....    MODE 1 RETURNS IONIZATION FRACTION /PARTITION FUNCTION
*....    MODE 2 RETURNS IONIZATION FRACTION
*....    MODE 3 RETURNS PARTITION FUNCTION
*....    MODE 4 RETURNS NUMBER OF ELECTRONS PRODUCED
*....    MODE 5 RETURNS ANSWER(ION) = PF   ANSWER(ION+7) = IP
*....    MODE + 10 RETURN ALL IONS TO NION.   MODE ALONE RETURN NION ONLY.
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.depart'
      include 'common.stateb'
      include 'common.tempbl'
*
*--------------------------- DUMMY ARGUMENTS ---------------------------
*
      double precision  answer (maxd, *)
      integer  j, iz, nion, mode
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  cf, charge, d1, d2, debye, dt, excess, f(6), g,
     &                  ip(6), p1, p2, part(6), pmin, potlo(6), potlow,
     &                  scale(4), t2000, tv, tvi, z
      integer  i, ion, it, k1, k2, k3, kp1, kscale, l, locz(29),
     &         mode1, n, nion2, nions, nnn(6, 365), nnn100
      logical  skip, rpf 
      save     locz, nnn, scale
*
*---------------------------- INITIALIZATION --------------------------
*
      data locz /  1,   3,   6,  10,  14, 18,  22,  27,  33,  39,  45,
     &            51,  57,  63,  69,  75,  81, 86,  91,  96, 101, 106,
     &           111, 116, 121, 126, 131, 136, 141 /
*
*....  ( 1)( 2)   ( 3)( 4)   ( 5)( 6)   ( 7)( 8)   ( 9)(10)   ( IP ) G  REF
      data ((nnn(i, n), i = 1, 6), n = 1, 17) /
     & 200020001, 200020011, 201620881, 231228281, 378953411,  1359502, d+f 1.00
     & 100010001, 100010001, 100010001, 100010001, 100010001,  1359500, g   1.01
     & 100010001, 100010011, 102111241, 145022061, 363059451,  2458104, d+f 2.00
     & 200020001, 200020071, 208524971, 382669341, 128222452,  5440302, d+f 2.01
     & 100010001, 100010001, 100010001, 100010001, 100010001,  5440300, g   2.02
     & 200020011, 201220481, 212922881, 258731081, 394251691,   538901, d+f 3.00
     & 100010001, 100010201, 126225521,  67216512, 351165562,  7561907, d+f 3.01
     & 200020001, 200020211, 227936571,  69610342, 137217102, 12241800, d+f 3.02
     & 100010001, 100010001, 100010001, 100010001, 100010001, 12241800, g   3.03
     & 100010051, 104311441, 131615641, 190623681, 298037691,   931900, ael 4.00
     & 200120231, 211422771, 249627631, 309034911, 398545051,  1820600, ael 4.01
     & 100010001, 100010201, 126225521,  67216512, 351165562, 15385000, ael 4.02
     & 200020001, 200020011, 201220661, 223426161, 332644691, 21765700, ael 4.03
     & 600060001, 600560281, 608761991, 637466191, 693973361,   829500, ael 5.00
     & 100310831, 132016901, 214226411, 315736741, 419147071,  2514900, ael 5.01
     & 200721061, 233526401, 297533311, 369040481, 440747651,  3792000, ael 5.02
     & 100010001, 100010001, 100010001, 100010001, 100010001, 25929800/ g   5.03
*
      data ((nnn(i,n), i = 1, 6), n =  18,  32) /
     & 893292271,  96110042, 105311262, 126315202, 196126432,  1125508, d+f 6.00
     & 595060251, 620865751, 713280191,  95712292, 167623542,  2437501, d+f 6.01
     & 105513201, 180324851, 341851341,  88416332, 296550722,  4787101, d+f 6.02
     & 204922771, 262630421, 350941931, 494556971, 644872001,  6447600, d+f 6.03
     & 403141851, 457051681, 594071181,  92913362, 203331152,  1452915, d+f 7.00
     & 919899541, 107211512, 124914302, 182526232, 403762662,  2959202, d+f 7.01
     & 596862721, 684177081,  88110342, 128317062, 239334312,  4742501, d+f 7.02
     & 112816481, 240733751, 462068491, 116419932, 283736822,  7744900, d+f 7.03
     & 210124681, 293634211, 391145791, 539862151, 703178471,  9786200, d+f 7.04
     & 874789691, 924795711,  99410492, 115213492, 169022242,  1361307, d+f 8.00
     & 424151091, 622874781,  91312832, 221842502,  79914013,  3510711, d+f 8.01
     &  95610702, 118113032, 149619922, 329761642, 101914173,  5488500, d+f 8.02
     & 603567171, 775391141, 106612482, 143716252, 181420032,  7739300, d+f 8.03
     & 124420321, 306943181, 606281181, 101712232, 142916342, 11387300, d+f 8.04
     & 215026541, 323137551, 421546491, 508255151, 594863811, 13807900/ ael 8.05
*
      data ((nnn(i,n), i = 1, 6), n =  33,  44) /
     & 575958511, 589859231, 595860671, 636470031, 815199581,  1741802, d+f 9.00
     & 900296401, 102610802, 113912542, 152921152, 318348952,  3498003, d+f 9.01
     & 469162651, 791295541, 121419552, 402686872, 154822203,  6264500, d+f 9.02
     &  99511422, 129214572, 170523002, 320140922, 498458762,  8713900, d+f 9.03
     & 615472711,  87710602, 127215002, 172919582, 218624152, 11421300, d+f 9.04
     & 135324181, 377252001, 661580261,  94410852, 122613672, 15711700, ael 9.05
     & 100010001, 100010051, 105313051, 210239461,  74013022,  2155808, d+f10.00
     & 580158751, 591759741, 642687101, 159332652,  64111533,  4106907, d+f10.01
     &  93510272, 110411662, 127116062, 257647882,  75110223,  6350000, d+f10.02
     & 529774371,  94611322, 135816202, 188221442, 240626682,  9701900, d+f10.03
     & 103312152, 140616092, 181320182, 222224262, 263128352, 12630000, ael10.04
     & 629178711,  98311802, 136715512, 173619202, 210422892, 15790900/ ael10.05
*
      data ((nnn(i,n), i = 1, 6), n =  45,  62) /
     & 200020001, 200320211, 207322131, 253031421, 417657451,   513802, d+f11.00
     & 100010001, 100010161, 119621261,  50711872, 246445382,  4728901, d+f11.01
     & 580158751, 591860351,  71813142, 321968812, 106014333,  7165000, d+f11.02
     &  96910772, 116012242, 130714232, 153916552, 177118872,  9888000, d+f11.03
     & 601386081, 108812932, 148916832, 187820722, 226624612, 13836900, ael11.04
     & 105712442, 144616652, 189221182, 234425702, 279630222, 17209000, ael11.05
     & 100010011, 101410621, 118414581, 204831781, 509479731,   764404, d+f12.00
     & 200120051, 202921001, 226926901, 368457091,  92814872,  1503101, d+f12.01
     & 100010001, 100110611, 177455431, 176546012,  99718753,  8011905, d+f12.02
     & 579758751, 591459501, 600560591, 611461681, 622362781, 10928900, ael12.03
     & 100611232, 120612752, 134214102, 147815462, 161416822, 14122900, ael12.04
     & 674896701, 121814462, 167018942, 211723412, 256527892, 18648900, ael12.05
     & 558857701, 583558761, 593260591, 635969541, 796790971,   598400, d+f13.00
     & 100310211, 110313021, 172828201,  55311252, 215637942,  1882203, d+f13.01
     & 200320201, 208622331, 250530971, 410251081, 611571211,  2844000, d+f13.02
     & 100010001, 100210881, 207436531, 523168101, 838999681, 11996000, d+f13.03
     & 577758651, 591259631, 604461351, 622563161, 640764981, 15377000, ael13.04
     & 103511582, 124713242, 140014772, 155316292, 170517812, 19042000/ ael13.05
*
      data ((nnn(i,n), i = 1, 6), n =  63,  74) /
     & 825189211,  95210052, 106211532, 134317202, 237934082,   814913, d+f14.00
     & 563057761, 588160311, 631768671, 791097651, 127817282,  1634000, d+f14.01
     & 101110771, 126716471, 232438081,  71914052, 262045302,  3346001, d+f14.02
     & 200720521, 217224081, 284439171, 551370951,  86810262,  4513000, d+f14.03
     & 100010001, 100210881, 207436531, 523168101, 838999681, 16672900, fak14.04
     & 575458521, 591459851, 610063201, 672674071, 843698661, 20510900, ael14.05
     & 402643441, 496757481, 658274401, 833492941, 103511532,  1048300, ael15.00
     & 874497931, 106011282, 119812802, 138415142, 164717802,  1972000, ael15.01
     & 564058061, 604164611, 709579551,  90410172, 112912422,  3015500, ael15.02
     & 100811411, 149720221, 280936121, 441552181, 602168241,  5135400, ael15.03
     & 200420781, 227025361, 281430911, 336936471, 392542021,  6500700, ael15.04
     & 100010001, 100010001, 100010001, 100010001, 100010001, 22041300/ g  15.05
*
      data ((nnn(i,n), i = 1, 6), n =  75,  90) /
     & 822887891, 930697831, 102610932, 121614492, 185124742,  1035708, d+f16.00
     & 443056011, 694982961,  96911522, 144218572, 227326892,  2339900, d+f16.01
     &  91610392, 113512242, 136416942, 233429882, 364242962,  3500000, d+f16.02
     & 560058861, 633871081,  82410062, 123314602, 168619132,  4728900, d+f16.03
     & 104512901, 177025421, 375163021, 122420462, 286036742,  7250000, d+f16.04
     & 202321571, 241428261, 358355061,  78310152, 124814802,  8802800, d+f16.05
     & 538155931, 571657911, 598067191,  89013782, 227737172,  1300916, d+f17.00
     & 873396771, 104411072, 118513532, 175525872, 406763932,  2379903, d+f17.01
     & 506569571,  87610522, 134421682, 439092662, 182132573,  3990006, d+f17.02
     &  95110872, 120013232, 154921252, 345149322, 641378942,  5350000, d+f17.03
     & 558960371, 677779341,  95311692, 138816082, 182720472,  6780000, d+f17.04
     & 100010001, 100010051, 106913911, 240147261,  90716112,  1575411, d+f18.00
     & 550256831, 578158781, 636585461, 151530162,  58010303,  2762007, d+f18.01
     &  92110362, 112412002, 133216772, 254443722,  76512833,  4090003, d+f18.02
     & 582082081, 103112292, 149920212, 309750502, 720793642,  5978900, d+f18.03
     &  97111072, 123213982, 172625622, 463976582, 106413633,  7500000/ d+f18.04
*
      data ((nnn(i,n), i = 1, 6), n =  91, 105) /
     & 200020011, 200720361, 211923291, 280137141, 525575741,   433803, d+f19.00
     & 100010001, 100110341, 135929551,  79119282, 405274892,  3180905, d+f19.01
     & 554657081, 581260301,  73012702, 285363872, 129023363,  4600005, d+f19.02
     &  96010862, 118413212, 180836632,  90321023, 416863253,  6090000, d+f19.03
     & 657793361, 119515082, 195826322, 352944302, 533162332,  8259900, d+f19.04
     & 100110061, 104311741, 145919971, 294345051,  69010322,   611003, d+f20.00
     & 205822781, 279234761, 427553061, 688994901, 136319772,  1186701, d+f20.01
     & 100010001, 100510821, 168744821, 130232522,  69012813,  5121003, d+f20.02
     & 555157161, 585662471,  82816862,  42510013, 168423663,  6700000, d+f20.03
     &  99411262, 123814062, 182930402, 484766392,  84310223,  8438900, d+f20.04
     & 924696691, 105212282, 151219062, 240530032, 368944512,   653900, ael21.00
     & 190424662, 297634542, 391743752, 482952832, 573761912,  1280000, ael21.01
     & 976799291, 101110322, 105810882, 111911502, 118112122,  2475000, ael21.02
     & 100010001, 100510821, 168744821, 130232522,  69012813,  7390000, fak21.03
     & 555157161, 585662471,  82816862,  42510013, 168423663,  9200000/ fak21.04
*
      data ((nnn(i,n), i = 1, 6), n = 106, 120) /
     & 181021172, 260333222, 430155582, 710089242, 110213293,   681900, d+f22.00
     & 474659872, 721284672,  98211413, 134515623, 177919963,  1356900, d+f22.01
     & 228327012, 308134272, 381143862, 534563472, 734983512,  2747000, d+f22.02
     & 971498311,  99210032, 102610572, 108711172, 114711782,  4324000, d+f22.03
     & 100010001, 100510821, 168744821, 130232522,  69012813,  9980000, fak22.04
     & 272835172, 425851532, 632278322,  97212013, 146817723,   674000, ael23.00
     & 373954132, 743597002, 121414713, 173920143, 229225713,  1464900, ael23.01
     & 323142642, 519660272, 679975352, 824789522,  96610363,  2930900, ael23.02
     & 248329302, 324234952, 373439752, 421744582, 469949412,  4800000, ael23.03
     & 970698231, 990699881, 100710152, 102410322, 104010482,  6500000, ael23.04
     & 717277611,  92911652, 152620872, 295141952, 550468122,   676400, d+f24.00
     &  71611552, 205635512, 558281952, 115315823, 205625293,  1649000, d+f24.01
     & 280639822, 538369722,  87610823, 129115003, 170919183,  3095000, d+f24.02
     & 377150952, 616070292, 791788382,  97610683, 116012523,  5000000, d+f24.03
     & 264730962, 341436462, 394042872, 463549832, 533056782,  7300000/ d+f24.04
*
      data ((nnn(i,n), i = 1, 6), n = 121, 135) /
     & 600060321, 629270891,  86911302, 151020222, 267534752,   743100, ael25.00
     & 739594821, 139921212, 309342852, 567372412,  97112553,  1563600, ael25.01
     &  98417472, 265535782, 454754842, 641973532, 828792212,  3369000, ael25.02
     & 328847052, 586668342, 771785912,  94710343, 112112093,  5300000, ael25.03
     & 422055132, 636770792, 779285062, 921999322, 106411363,  7600000, ael25.04
     & 197023222, 274433302, 416753952, 723799822, 139419053,   787038, d+f26.00
     & 409453722, 686687452, 110213823, 174322233, 286437043,  1617902, d+f26.01
     & 262136422, 501167232,  87911303, 138916483, 190721673,  3064300, d+f26.02
     &  98723522, 420363072,  87011423, 145117913, 215925463,  5700000, ael26.03
     & 388854482, 666275742, 846693572, 102511143, 120312923,  7900000, d+f26.04
     & 199427202, 335740022, 474957182, 708090462, 118315403,   786000, d+f27.00
     & 279739202, 490858232, 684582472, 104713233, 159818733,  1704900, d+f27.01
     & 279836622, 461857562, 720693022, 124915873, 192522633,  3349000, d+f27.02
     & 262136422, 501167232,  87911303, 138916483, 190821673,  5300000, fak27.03
     &  98723522, 420363072,  87011423, 145117913, 215925463,  8300000/ fak27.04
*
      data ((nnn(i,n), i = 1, 6), n = 136, 149) /
     & 227027622, 306233052, 356839222, 446052912, 652382292,   763314, d+f28.00
     & 108416342, 222428472, 353944332, 577378932, 110314303,  1814900, d+f28.01
     & 198724282, 293236452, 468362702,  86511123, 136016073,  3516000, d+f28.02
     & 279836622, 461857562, 720693022, 124915873, 192522633,  5600000, fak28.03
     & 262136422, 501167232,  87911303, 138916483, 190721673,  7900000, fak28.04
     & 201620781, 231026761, 314737361, 450555381, 692386911,   772301, d+f29.00
     & 109415761, 247938311,  58910042, 190937022,  68311693,  2028903, d+f29.01
     & 897195961, 107212972, 165021182, 260230862, 356940532,  3682900, d+f29.02
     & 100010001, 100410231, 108712611, 167124841, 388460411,   939102, d+f30.00
     & 200020021, 201620761, 223726341, 351352061,  80812472,  1796001, d+f30.01
     & 100610471, 122617301, 300566361, 149924112, 332342352,  3970000, d+f30.02
     & 403245601, 493151431, 529654331, 559358091, 611065171,   600000, ael31.00
     &  99710051, 104511541, 135016501, 208226431, 321837921,  2050900, ael31.01
     & 199820071, 204521391, 229124761, 266028451, 302932131,  3070000/ ael31.02
*
      data ((nnn(i,n), i = 1, 6), n = 150, 164) /
     & 502665261, 755183501, 901496201, 102410942, 117912812,   787900, ael32.00
     & 422848161, 512153401, 557458941, 636270361, 794489061,  1593000, ael32.01
     & 100010261, 114613921, 175221251, 249828711, 324436181,  3421000, ael32.02
     & 403143241, 491856701, 649173781, 840396751, 113013392,   981000, ael33.00
     & 593676641, 884697521, 105911572, 129515012, 180322212,  1858700, ael33.01
     & 484470541,  91510972, 125614082, 157017612, 199722912,  2829900, ael33.02
     & 630172361, 799686381, 919797221, 102810942, 117712832,   975000, ael34.00
     & 438055511, 691582151,  94510732, 121413672, 152016732,  2150000, ael34.01
     & 651982921,  94610382, 113212492, 139515462, 169718482,  3200000, ael34.02
     & 437347431, 498951671, 538559501,  74710812, 169126672,  1183910, d+f35.00
     & 705183611,  93510092, 111614162, 222932532, 427652992,  2160000, d+f35.01
     & 510869921,  87410312, 123116552, 236530712, 377744832,  3590000, d+f35.02
     & 100010001, 100010051, 105012781, 198535971,  65911422,  1399507, d+f36.00
     & 461049811, 522254261, 609088131, 168935052,  68612253,  2455908, d+f36.01
     & 759990901, 101911142, 129017782, 302856642,  99414333,  3690000/ d+f36.02
*
      data ((nnn(i,n), i = 1, 6), n = 165, 179) /
     & 200020011, 200720361, 211523021, 269434141, 459163351,   417502, d+f37.00
     & 100010001, 100110321, 129524961,  61014202, 291753192,  2750004, d+f37.01
     & 473650891, 533156051,  66810932, 232950852,  99915303,  4000000, d+f37.02
     & 100110041, 104111741, 146019721, 281941411, 607785251,   569202, d+f38.00
     & 202621931, 255331271, 384347931, 624085761, 122417632,  1102600, d+f38.01
     & 100010001, 100110321, 129524961,  61014202, 291753192,  4300000, fak38.02
     & 791587851, 100012192, 155119942, 254031782, 389946932,   637900, ael39.00
     & 118217102, 220827002, 319036792, 416646512, 513256072,  1223000, ael39.01
     &  92510012, 104710862, 112311612, 120212472, 132814282,  2050000, ael39.02
     & 141320802, 291439702, 531170262,  92712273, 162521053,   684000, d+f40.00
     & 354454352, 724689652, 107212643, 148517093, 193321573,  1312900, d+f40.01
     & 209727032, 324537052, 415446282, 510255752, 604965222,  2298000, d+f40.02
     & 256636022, 465759302, 749693962, 116514243, 171520333,   687900, ael41.00
     & 335157222,  84511463, 147718363, 221826083, 299933893,  1431900, ael41.01
     & 223725352, 280830972, 340937362, 406844002, 473150632,  2503900/ ael41.02
*
      data ((nnn(i,n), i = 1, 6), n = 180, 194) /
     & 703972941,  82610822, 154822682, 327244912, 571469372,   709900, d+f42.00
     &  75714552, 274347322, 718897632, 123414913, 174920063,  1614900, d+f42.01
     & 267645462, 669890262, 115514323, 173620673, 242528083,  2714900, ael42.02
     &  90613732, 184823562, 291735332, 419949102, 565764332,   728000, ael43.00
     & 131318312, 227126932, 311735452, 397644072, 483852692,  1525900, ael43.01
     & 204721673, 234725733, 284031463, 348738613, 426546943,  3000000, ael43.02
     & 176824122, 318941082, 515263202, 761790472, 106112303,   736400, ael44.00
     & 221934642, 501968372,  88911173, 136316243, 189221613,  1675900, ael44.01
     & 210622722, 241025422, 267928262, 297731272, 327834282,  2846000, ael44.02
     & 148520202, 255230902, 364942462, 489656082, 638872352,   746000, ael45.00
     & 153421292, 288137912, 484660322, 720187062, 101011483,  1807000, ael45.01
     & 254537212, 492362292, 770592182, 107312243, 137615273,  3104900, ael45.02
     & 115919651, 320746011, 607576761,  95011642, 141817172,   832900, ael46.00
     & 755087211, 105913442, 173122222, 282034722, 412247732,  1941900, ael46.01
     & 180223462, 289735212, 414247632, 538460052, 662672472,  3292000/ ael46.02
*
      data ((nnn(i,n), i = 1, 6), n = 195, 209) /
     & 200020001, 200220141, 206422141, 257633021, 455164681,   757403, d+f47.00
     & 100810581, 125817401, 260641031,  66210072, 135316982,  2148000, d+f47.01
     & 795887491,  97711762, 156620252, 248329422, 340038582,  3481900, d+f47.02
     & 100010001, 100410241, 109212891, 176827421, 444268771,   899003, d+f48.00
     & 200020021, 201720921, 233329881, 451475371, 127520782,  1690301, d+f48.01
     & 100310281, 114815371, 246138311, 519265531, 791492761,  3747000, d+f48.02
     & 252431921, 368440461, 433746521, 512259221, 723389021,   578400, d+f49.00
     & 100110071, 104611651, 146118581, 225426511, 304734431,  1886000, d+f49.01
     & 200120111, 205021611, 243628031, 317035371, 390442701,  2802900, d+f49.02
     & 232637101, 488058571, 669074381, 816189091,  97210632,   734200, ael50.00
     & 286335941, 408144471, 479351961, 571862901, 686274341,  1462700, ael50.01
     & 100010251, 114013811, 175321601, 256829751, 338337901,  3049000, ael50.02
     & 404043481, 494656811, 646772781, 813490751, 101411372,   863900, ael51.00
     & 303147981, 618472951, 827392621, 103711702, 131214532,  1650000, ael51.01
     & 313037601, 429347901, 536260591, 689477591, 862494881,  2529900/ ael51.02
*
      data ((nnn(i,n), i = 1, 6), n = 210, 224) /
     & 526258801, 657372351, 784284071, 897095741, 102711082,   900900, ael52.00
     & 440855541, 686481251,  93810792, 125414792, 176321132,  1860000, ael52.01
     & 349054751, 699883081,  96611302, 134216202, 197724212,  2800000, ael52.02
     & 405342041, 438645621, 475751071, 587974491, 102214572,  1045404, d+f53.00
     & 568567471, 773485861,  94510362, 112712182, 130914002,  1909000, d+f53.01
     & 514269581,  86910562, 130716652, 215327742, 351843662,  3200000, ael53.02
     & 100010001, 100010091, 109515351, 291060661, 119621482,  1212716, d+f54.00
     & 414844131, 465649111, 538464651,  87112232, 158019362,  2120000, d+f54.01
     & 615475101, 867797531, 112213462, 157618062, 203622662,  3209900, d+f54.02
     & 200020001, 201020501, 215623871, 283536181, 462756261,   389300, d+f55.00
     & 100010001, 100310371, 119016501, 269146361,  77912412,  2510000, d+f55.01
     & 424445601, 481750061, 516953311, 549356551, 581759791,  3500000, d+f55.02
     & 101210791, 135119351, 282340571, 574580391, 111015062,   521002, d+f56.00
     & 262638611, 504160621, 698579371,  91010692, 129115952,  1000000, d+f56.01
     & 100010001, 100310351, 118416321, 264945521,  76512182,  3700000/ fak56.02
*
      data ((nnn(i,n), i = 1, 6), n = 225, 239) /
     &  71111992, 172323592, 312540402, 510763182, 765791012,   558000, ael57.00
     & 204529582, 383647882, 582469262, 807992692, 104911723,  1106000, ael57.01
     &  94712552, 148416582, 179819212, 203621522, 227424042,  1916900, ael57.02
     & 295959132, 103515693, 215527593, 335939413, 449650223,   565000, ael58.00
     &  79718153, 289639443, 495159253, 686877533, 863794813,  1085000, ael58.01
     & 298640242, 475053692, 596965912, 725379692, 872094692,  2008000, ael58.02
     & 460693672, 158523823, 327242303, 519661563, 709379783,   541900, fak59.00
     & 455480232, 114014653, 178521013, 240927073, 299232633,  1055000, ael59.01
     &  46410533, 183826893, 354443773, 518459633, 674375243,  2320000, ael59.02
     & 139623042, 364860002,  96114603, 209828633, 373446973,   549000, ael60.00
     & 460493692, 158523823, 327142303, 519661563, 709279783,  1073000, ael60.01
     & 455480232, 114014653, 178521013, 240927073, 299232633,  2000000, fak60.02
     & 131720482, 280535692, 441254492, 676583972, 103412583,   555000, ael61.00
     & 139623042, 364860002,  96114603, 209828633, 373446973,  1089900, fak61.01
     & 460493682, 158523823, 327142303, 519661563, 709279783,  2000000/ fak61.02
*
      data ((nnn(i,n), i = 1, 6), n = 240, 254) /
     &  92915672, 222431062, 444763802,  89612173, 159520253,   562900, ael62.00
     & 315059662,  97114563, 204627093, 342541693, 490556383,  1106900, ael62.01
     & 269037812, 520270372,  91111273, 133915483, 172719093,  2000000, ael62.02
     & 800080571, 851699301, 127617362, 240433032, 444958442,   568000, ael63.00
     & 125416052, 211828182, 375549622, 644381732, 101112213,  1125000, ael63.01
     & 800080571, 851699301, 127617362, 240433032, 444958442,  2000000, fak63.02
     & 240432982, 427555202, 708489962, 112613853, 167319843,   615900, ael64.00
     & 534793262, 139219123, 247730843, 371043333, 495055893,  1210000, ael64.01
     & 364145232, 514756362, 604864112, 673870372, 732276072,  2000000, ael64.02
     & 480767202,  89011393, 144118243, 230028753, 354142883,   584900, ael65.00
     & 480767192,  89011393, 144118243, 230028753, 354142883,  1151900, fak65.01
     & 480767202,  89011393, 144118243, 230028753, 354142883,  2000000, fak65.02
     & 343147532, 645887152, 115314793, 183322063, 257729373,   593000, fak66.00
     & 343147532, 645887142, 115314793, 183322063, 257729373,  1167000, ael66.01
     & 343147532, 645887142, 115314793, 183322063, 257729373,  2000000/ fak66.02
*
      data ((nnn(i,n), i = 1, 6), n = 255, 269) /
     & 222635002, 542276772, 100312353, 145716713, 187020703,   602000, fak67.00
     & 222635002, 542276772, 100312353, 145716713, 187020703,  1180000, fak67.01
     & 222635002, 542276772, 100312353, 145716713, 187020703,  2000000, ael67.02
     & 133715382, 209130152, 429859382,  79410293, 129815983,   609900, ael68.00
     & 265934782, 497877532, 120517733, 245032063, 400448073,  1193000, ael68.01
     & 265934782, 497877532, 120517733, 245032063, 400448073,  2000000, fak68.02
     & 800381111,  87510702, 147621462, 310343462, 585475982,   618000, ael69.00
     & 156718872, 279244452, 678196342, 128316243, 197823443,  1205000, ael69.01
     &  93517192, 364666132, 103414613, 192624193, 293334613,  2370000, ael69.02
     & 100010011, 101310651, 118613951, 169120661, 250629971,   625000, ael70.00
     & 200120901, 270345231,  81714042, 223533112, 461959862,  1217000, ael70.01
     & 100312561, 250851931,  91914182, 198626022, 323638692,  2000000, ael70.02
     & 514664441, 759086851,  99211442, 133315612, 182721252,   609900, ael71.00
     & 125924831, 438667801,  98714112, 199727872, 380850742,  1389900, ael71.01
     & 323948621, 661297271, 158626482, 426865032,  93712843,  1900000/ ael71.02
*
      data ((nnn(i,n), i = 1, 6), n = 270, 284) /
     & 659294081, 128016962, 222528952, 372047062, 585171462,   700000, ael72.00
     &  99117882, 274638812, 520867322,  84410313, 123314453,  1489900, ael72.01
     & 187427702, 343739872, 448049452, 539358282, 625266642,  2329900, ael72.02
     &  65210892, 171325762, 373552252, 705192012, 116414343,   787900, ael73.00
     & 192837842, 600784802, 111113823, 165419233, 218524383,  1620000, ael73.01
     &  99117872, 274638812, 520867312,  84410313, 123314453,  2400000, fak73.02
     & 398981651, 130019172, 273438022, 516168382,  88411163,   797900, ael74.00
     & 131429482, 523279952, 111414623, 183422233, 262130233,  1770000, ael74.01
     & 192837842, 600784792, 111113823, 165419233, 218524383,  2500000, fak74.02
     & 600963001,  75910412, 150121572, 301940972, 539168952,   787000, ael75.00
     &  73710852, 190731262, 464964142,  83810503, 127315053,  1660000, ael75.01
     & 131429482, 523279952, 111414623, 183422233, 262130233,  2600000, fak75.02
     & 110815502, 216829732, 398752322, 672484682, 104612673,   850000, ael76.00
     & 168225972, 362046562, 566766422, 757484612,  93010103,  1700000, ael76.01
     &  73710852, 190731262, 464964142,  83810503, 127315053,  2700000/ fak76.02
*
      data ((nnn(i,n), i = 1, 6), n = 285, 299) /
     & 129117892, 239430882, 388748292, 596173252,  89510843,   910000, ael77.00
     & 110815502, 216829732, 398752322, 672484682, 104612673,  2000000, fak77.01
     & 168225972, 362046562, 566766422, 757484612,  93010103,  2800000, fak77.02
     & 158918512, 207523002, 254328242, 316335762, 407246582,   900000, ael78.00
     &  98115462, 224930742, 401150612, 623475412,  89910583,  1855900, ael78.01
     & 110815502, 216829732, 398752322, 672484682, 104612673,  2900000, fak78.02
     & 203222611, 265731251, 364042301, 494958601, 702084731,   922000, ael79.00
     & 120521331, 357753801,  75310062, 130516572, 206925452,  2050000, ael79.01
     & 651780821, 108814772, 195925252, 316338622, 460853882,  3000000, ael79.02
     & 100010001, 100110111, 105211851, 152122101, 341552811,  1043002, d+f80.00
     & 200320211, 210023021, 268834231, 480472341, 111416912,  1875000, d+f80.01
     & 104012871, 186129471, 458664151,  82410072, 119013732,  3420000, d+f80.02
     & 200420711, 222424271, 265429161, 325637371, 442853911,   610500, ael81.00
     & 100010021, 101910801, 121414641, 189525811, 358949721,  2041900, ael81.01
     & 200020311, 216624611, 296337451, 489064791,  85711212,  2979900/ ael81.02
*
      data ((nnn(i,n), i = 1, 6), n = 300, 314) /
     & 103411711, 147819101, 244331781, 434862751,  93113762,   741404, d+f82.00
     & 204122231, 248227841, 311535621, 429153941, 651976431,  1502800, d+f82.01
     & 100210131, 106812201, 154522671, 381665951,  95512512,  3192900, d+f82.02
     & 400140351, 416944121, 474851591, 564362181, 690477231,   728700, ael83.00
     & 106814451, 204427341, 350744811, 586879131, 108314772,  1667900, ael83.01
     & 205523051, 264830231, 345439921, 469156001, 675281671,  2555900, ael83.02
     & 500950661, 518153561, 559058941, 628968071, 748483501,   843000, ael84.00
     & 443756241, 696282451,  95411012, 128615262, 182922012,  1900000, fak84.01
     & 336953201, 682481011,  93810882, 127915272, 184622442,  2700000, fak84.02
     & 402841621, 431544771, 463148311, 520059491, 734896851,   930000, fak85.00
     & 576168741, 788387631,  96910642, 116012552, 135014462,  2000000, fak85.01
     & 490265341, 812797201, 116614322, 179622692, 285035302,  2900000, fak85.02
     & 100010001, 100010031, 102311051, 133018071, 264539391,  1074500, ael86.00
     & 402841621, 431544771, 463148311, 520059491, 734996851,  2000000, fak86.01
     & 576168741, 788387631,  96910642, 116012552, 135014462,  3000000/ fak86.02
*
      data ((nnn(i,n), i = 1, 6), n = 315, 329) /
     & 200020011, 201220591, 218124481, 296538611, 488859141,   400000, fak87.00
     & 100010001, 100010031, 102311051, 133018071, 264539401,  2200000, fak87.01
     & 421645151, 477449611, 511852711, 542455761, 572958821,  3300000, fak87.02
     & 100010041, 105212131, 153220271, 270435641, 460258111,   527600, ael88.00
     & 201221791, 258131471, 381645781, 546365131, 777592781,  1014400, ael88.01
     & 100010001, 100010031, 102311051, 133018071, 264539391,  3400000, fak88.02
     & 510064491,  82710872, 142718412, 232328712, 348341572,   690000, ael89.00
     & 228951571,  88513232, 183324132, 305537492, 448152402,  1210000, ael89.01
     & 723989131, 103511752, 130814352, 155416652, 177018682,  2000000, ael89.02
     & 620099241, 162725772, 391457072,  80110833, 141818023,   600000, ael90.00
     & 620099241, 162725772, 391457072,  80110833, 141818023,  1200000, fak90.01
     & 620099251, 162725772, 391457072,  80110833, 141818023,  2000000, fak90.02
     & 347877992, 129318323, 240730533, 380546863, 570368573,   600000, ael91.00
     & 347877992, 129318323, 240730533, 380546863, 570368573,  1200000, fak91.01
     & 347777992, 129318323, 240730533, 380546863, 570368573,  2000000/ fak91.02
*
      data ((nnn(i,n), i = 1, 6), n = 330, 344) /
     & 209530092, 450866762,  96613623, 186524763, 318839893,   600000, ael92.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak92.01
     & 209530092, 450866762,  96613623, 186524763, 318839893,  2000000, fak92.02
     & 209530092, 450866762,  96613623, 186524763, 318839893,   600000, fak93.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak93.01
     & 209530092, 450866762,  96613623, 186524763, 318839893,  2000000, fak93.02
     & 209530092, 450866762,  96613623, 186524763, 318839893,   600000, fak94.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak94.01
     & 209530092, 450866762,  96613623, 186524763, 318839893,  2000000, fak94.02
     & 209530092, 450866762,  96613623, 186524763, 318839893,   600000, fak95.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak95.01
     & 209530092, 450866762,  96613623, 186524763, 318839893,  2000000, fak95.02
     & 209530092, 450866762,  96613623, 186524763, 318839893,   600000, fak96.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak96.01
     & 209530092, 450866762,  96613623, 186524763, 318839893,  2000000/ fak96.02
*
      data ((nnn(i,n), i = 1, 6), n = 345, 353) /
     & 209530092, 450866762,  96613623, 186524763, 318839893,   600000, fak97.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak97.01
     & 209530092, 450866762,  96613623, 186524763, 318839893,  2000000, fak97.02
     & 209530092, 450866762,  96613623, 186524763, 318839893,   600000, fak98.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak98.01
     8 209530092, 450866762,  96613623, 186524763, 318839893,  2000000, fak98.02
     9 209530092, 450866762,  96613623, 186524763, 318839893,   600000, fak99.00
     & 209530092, 450866762,  96613623, 186524763, 318839893,  1200000, fak99.01
     & 209530092, 450866762,  96613623, 186524763, 318839893,  2000000/ fak99.02
*
      data ((nnn(i,n), i = 1, 6), n = 354, 365) /
     & 893292271,  96110042, 105311262, 126315202, 196126432,  1125508, d+f 6.00
     & 595060251, 620865751, 713280191,  95712292, 167623542,  2437501, d+f 6.01
     & 105513201, 180324851, 341851341,  88416332, 296550722,  4787101, d+f 6.02
     & 204922771, 262630421, 350941931, 494556971, 644872001,  6447600, d+f 6.03
     & 100010001, 100010001, 100010001, 100010001, 100010001, 39207700, g   6.04
     & 200020001, 200020001, 200020001, 200020001, 200020001, 48998100, g   6.05
     & 403141851, 457051681, 594071181,  92913362, 203331152,  1452915, d+f 7.00
     & 919899541, 107211512, 124914302, 182526232, 403762662,  2959202, d+f 7.01
     & 596862721, 684177081,  88110342, 128317062, 239334312,  4742501, d+f 7.02
     & 112816481, 240733751, 462068491, 116419932, 283736822,  7744900, d+f 7.03
     & 210124681, 293634211, 391145791, 539862151, 703178471,  9786200, d+f 7.04
     & 100010001, 100010001, 100010001, 100010001, 100010001, 55205700/ g   7.05
*
      data scale / 0.001, 0.01, 0.1, 1.0 /

*
*------------------------------- EXECUTION -----------------------------
*
#ifdef ODF
      rpf = .true.
#else
      rpf = .false.
#endif


      mode1 = mode
      if(mode1 .gt. 10) mode1 = mode1 - 10
*
*.... LOWERING OF THE IONIZATION POTENTIAL IN VOLTS FOR UNIT ZEFF
*
      charge = 2.0d0 * xne(j)
      excess = charge - p(j) / tk(j)
*
*.... ALLOWANCE FOR DOUBLY IONIZED HELIUM
*
      if(excess .gt. 0.0d0) charge = charge + 2.0 * excess
      debye  = sqrt(tk(j) / 2.8965d-18 / charge)
      potlow = min(1.0d0, 1.44d-7 / debye)
      tv     = tkev(j)
      tvi    = 1.0d0 / tv
*
      if (iz .gt. 28) then
         n     = 3 * iz + 54
         nions = 3
*
      else if (iz .eq. 6) then
         n     = 354
         nions = 6
*
      else if (iz .eq. 7) then
         n     = 360
         nions = 6
      else
         n     = locz(iz)
         nions = locz(iz + 1) - n
      end if
*
      nion2 = min(nion + 2, nions)
      n     = n - 1
*
      do ion = 1, nion2
         z          = ion
         potlo(ion) = potlow * z
         n          = n + 1
         nnn100     = nnn(6, n) / 100
         ip(ion)    = dble(nnn100) * 1.0d-3
         g          = nnn(6, n) - nnn100 * 100
*
      if (rpf .and. (iz .ge. 20) .and. (iz .lt. 29)) then 
        call pfiron(iz, ion, tlog(j)/2.30258509288405,
     &               potlo(ion)*8065.479, part(ion))
        
      else
 
          if(n .eq. 1) then
            skip    = .false.
            part(1) = 2.0d0 * bhyd(j, 1)
            part(1) = part(1) +  
     &                 8.0d0 * bhyd(j, 2) * exp( -10.196d0 * tvi) + 
     &                18.0d0 * bhyd(j, 3) * exp( -12.084d0 * tvi) + 
     &                32.0d0 * bhyd(j, 4) * exp( -12.745d0 * tvi) + 
     &                50.0d0 * bhyd(j, 5) * exp( -13.051d0 * tvi) + 
     &                72.0d0 * bhyd(j, 6) * exp( -13.217d0 * tvi)
            d1      = 13.595d0 / 6.5d0 / 6.5d0 * tvi
            d2      = potlo(1) * tvi
*
         else
            t2000  = ip(ion) * 2000.0d0 / 11.0d0
            it     = max(1, min(9,  int(t(j) / t2000 - 0.5d0)))
            dt     = t(j) / t2000 - dble(it) - 0.5d0
            pmin   = 1.0d0
            i      = (it + 1) / 2
            k1     = nnn(i, n) / 100000
            k2     = nnn(i, n) - k1 * 100000
            k3     = k2 / 10
            kscale = k2 - k3 * 10
*
            if(mod(it, 2) .eq. 0) then
               p1     = dble(k3) * scale(kscale)
               k1     = nnn(i + 1, n) / 100000
               kscale = mod(nnn(i + 1, n), 10)
               p2     = dble(k1) * scale(kscale)
*
            else
               p1 = dble(k1) * scale(kscale)
               p2 = dble(k3) * scale(kscale)
*
               if(dt .lt. 0.0d0 .and. kscale .le. 1)then
                  kp1 = p1
                  if(kp1 .eq. int(p2 + 0.5d0)) pmin = kp1
               end if
*
            end if
*
            part(ion) = max(pmin, p1 + (p2 - p1) * dt)
*
            if(   g       .eq. 0.0d0 .or. 
     &         potlo(ion) .lt. 0.1d0 .or. 
     &           t(j)     .lt. t2000 * 4.0d0) then
               skip = .true.
*
            else
               skip = .false.
               if(t(j) .gt. (t2000 * 11.0d0)) tv = (t2000 * 11.0d0) * 
     &                                              8.6171d-5
               tvi = 1.0d0 / tv
               d1 = 0.1d0 * tvi
            end if
*
         end if
*
         if(.not. skip) then
            d2        = potlo(ion) * tvi
            part(ion) = part(ion) + g * exp( - ip(ion) * tvi) * 
     &                  (sqrt(13.595d0 * z * z * tvi / d2) ** 3 * 
     &                  (1.0d0 / 3.0d0 + (1.0d0 - (0.5d0 + (1.0d0 /
     &                  18.0d0 + d2 / 120.0d0) * d2) * d2) * d2) - 
     &                   sqrt(13.595d0 * z * z * tvi / d1) ** 3 * 
     &                   (1.0d0 / 3.0d0 + (1.0d0 - (0.5d0 + (1.0d0 / 
     &                  18.0d0 + d1 / 120.0d0) * d1) * d1) * d1))
            tv  = tkev(j)
            tvi = 1.0d0 / tv
         end if
*
       end if

      end do
*
      if(mode1 .eq. 5) then
         answer(7, 1) = 0.0d0
*
         do ion = 1, nion
            answer(ion, 1)     = part(ion)
            answer(ion + 7, 1) = ip(ion) + answer(ion + 6, 1)
         end do
*
      else
*
         if(mode1 .ne. 3) then
            n  = n - nion2
            cf = 2.0d0 * 2.4148d15 * t(j) * sqrt(t(j)) / xne(j)
*
            do ion = 2, nion2
               n      = n + 1
               f(ion) = cf * part(ion) / part(ion - 1) * 
     &            exp(-min((ip(ion-1) - potlo(ion-1))/tv,75.0d0))
*
*.... THE CUTOFF AT EXP(-75.0) IS TO BE CONSISTENT WITH BOB'S VERSION.
*.... IT IS NOT NEEDED ON SUN IN DOUBLE PRECISION, WHERE UNDERFLOW 
*.... STARTS AT EXP(-709).
*
            end do
*
            f(1) = 1.0d0
            l    = nion2 + 1
*
            do ion = 2, nion2
               l    = l - 1
               f(1) = 1.0d0 + f(l) * f(1)
            end do
*
            f(1) = 1.0d0 / f(1)
*
            do ion = 2, nion2
               f(ion) = f(ion - 1) * f(ion)
            end do
*
         end if
*
         if(mode .ge. 10) then
*
            if(mode1 .eq. 1) then
*
               do ion = 1, nion
                  answer(j, ion) = f(ion) / part(ion)
               end do
*
            else if(mode1 .eq. 2) then
*
               do ion = 1, nion
                  answer(j, ion) = f(ion)
               end do
*
            else if(mode1 .eq. 3) then
*
               do ion = 1, nion
                  answer(j, ion) = part(ion)
               end do
*
            else if(mode1 .eq. 4) then
               answer(j, 1) = 0.0d0
*
               do ion = 2, nion2
                  answer(j, 1) = answer(j, 1) + f(ion) * dble(ion - 1)
               end do
*
            end if
*
         else
*
            if(mode1 .eq. 1) then
               answer(j, 1) = f(nion) / part(nion)
*
            else if(mode1 .eq. 2) then
               answer(j, 1) = f(nion)
*
            else if(mode1 .eq. 3) then
               answer(j, 1) = part(nion)
*
            else if(mode1 .eq. 4) then
               answer(j, 1) = 0.0d0
*
               do ion = 2, nion2
                  answer(j, 1) = answer(j, 1) + f(ion) * dble(ion - 1)
               end do
*
            end if
*
         end if
*
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   P F S A H A ************
*

      subroutine pfiron(nelem,ion,tlog8,potlow8,pf)
      implicit none
!      include 'common.tempbl'

      double precision  tlog8,potlow8,pf, potlow, it, f
      double precision  tlog, p
      integer low, ion, nelem 
      double precision potlo, potlolog, pftab, p63

      dimension potlo(7),potlolog(7)
      dimension pftab(7,56,10,9),p63(63,560)
#ifdef ODF 
      equivalence (pftab(1,1,1,1),p63(1,1))

      double precision pf001(63),pf002(63),pf003(63),pf004(63),pf005(63)
      double precision pf006(63),pf007(63),pf008(63),pf009(63),pf010(63)
      double precision pf011(63),pf012(63),pf013(63),pf014(63),pf015(63)
      double precision pf016(63),pf017(63),pf018(63),pf019(63),pf020(63)
      double precision pf021(63),pf022(63),pf023(63),pf024(63),pf025(63)
      double precision pf026(63),pf027(63),pf028(63),pf029(63),pf030(63)
      double precision pf031(63),pf032(63),pf033(63),pf034(63),pf035(63)
      double precision pf036(63),pf037(63),pf038(63),pf039(63),pf040(63)
      double precision pf041(63),pf042(63),pf043(63),pf044(63),pf045(63)
      double precision pf046(63),pf047(63),pf048(63),pf049(63),pf050(63)
      double precision pf051(63),pf052(63),pf053(63),pf054(63),pf055(63)
      double precision pf056(63),pf057(63),pf058(63),pf059(63),pf060(63)
      double precision pf061(63),pf062(63),pf063(63),pf064(63),pf065(63)
      double precision pf066(63),pf067(63),pf068(63),pf069(63),pf070(63)
      double precision pf071(63),pf072(63),pf073(63),pf074(63),pf075(63)
      double precision pf076(63),pf077(63),pf078(63),pf079(63),pf080(63)
      double precision pf081(63),pf082(63),pf083(63),pf084(63),pf085(63)
      double precision pf086(63),pf087(63),pf088(63),pf089(63),pf090(63)
      double precision pf091(63),pf092(63),pf093(63),pf094(63),pf095(63)
      double precision pf096(63),pf097(63),pf098(63),pf099(63),pf100(63)
      double precision pf101(63),pf102(63),pf103(63),pf104(63),pf105(63)
      double precision pf106(63),pf107(63),pf108(63),pf109(63),pf110(63)
      double precision pf111(63),pf112(63),pf113(63),pf114(63),pf115(63)
      double precision pf116(63),pf117(63),pf118(63),pf119(63),pf120(63)
      double precision pf121(63),pf122(63),pf123(63),pf124(63),pf125(63)
      double precision pf126(63),pf127(63),pf128(63),pf129(63),pf130(63)
      double precision pf131(63),pf132(63),pf133(63),pf134(63),pf135(63)
      double precision pf136(63),pf137(63),pf138(63),pf139(63),pf140(63)
      double precision pf141(63),pf142(63),pf143(63),pf144(63),pf145(63)
      double precision pf146(63),pf147(63),pf148(63),pf149(63),pf150(63)
      double precision pf151(63),pf152(63),pf153(63),pf154(63),pf155(63)
      double precision pf156(63),pf157(63),pf158(63),pf159(63),pf160(63)
      double precision pf161(63),pf162(63),pf163(63),pf164(63),pf165(63)
      double precision pf166(63),pf167(63),pf168(63),pf169(63),pf170(63)
      double precision pf171(63),pf172(63),pf173(63),pf174(63),pf175(63)
      double precision pf176(63),pf177(63),pf178(63),pf179(63),pf180(63)
      double precision pf181(63),pf182(63),pf183(63),pf184(63),pf185(63)
      double precision pf186(63),pf187(63),pf188(63),pf189(63),pf190(63)
      double precision pf191(63),pf192(63),pf193(63),pf194(63),pf195(63)
      double precision pf196(63),pf197(63),pf198(63),pf199(63),pf200(63)
      double precision pf201(63),pf202(63),pf203(63),pf204(63),pf205(63)
      double precision pf206(63),pf207(63),pf208(63),pf209(63),pf210(63)
      double precision pf211(63),pf212(63),pf213(63),pf214(63),pf215(63)
      double precision pf216(63),pf217(63),pf218(63),pf219(63),pf220(63)
      double precision pf221(63),pf222(63),pf223(63),pf224(63),pf225(63)
      double precision pf226(63),pf227(63),pf228(63),pf229(63),pf230(63)
      double precision pf231(63),pf232(63),pf233(63),pf234(63),pf235(63)
      double precision pf236(63),pf237(63),pf238(63),pf239(63),pf240(63)
      double precision pf241(63),pf242(63),pf243(63),pf244(63),pf245(63)
      double precision pf246(63),pf247(63),pf248(63),pf249(63),pf250(63)
      double precision pf251(63),pf252(63),pf253(63),pf254(63),pf255(63)
      double precision pf256(63),pf257(63),pf258(63),pf259(63),pf260(63)
      double precision pf261(63),pf262(63),pf263(63),pf264(63),pf265(63)
      double precision pf266(63),pf267(63),pf268(63),pf269(63),pf270(63)
      double precision pf271(63),pf272(63),pf273(63),pf274(63),pf275(63)
      double precision pf276(63),pf277(63),pf278(63),pf279(63),pf280(63)
      double precision pf281(63),pf282(63),pf283(63),pf284(63),pf285(63)
      double precision pf286(63),pf287(63),pf288(63),pf289(63),pf290(63)
      double precision pf291(63),pf292(63),pf293(63),pf294(63),pf295(63)
      double precision pf296(63),pf297(63),pf298(63),pf299(63),pf300(63)
      double precision pf301(63),pf302(63),pf303(63),pf304(63),pf305(63)
      double precision pf306(63),pf307(63),pf308(63),pf309(63),pf310(63)
      double precision pf311(63),pf312(63),pf313(63),pf314(63),pf315(63)
      double precision pf316(63),pf317(63),pf318(63),pf319(63),pf320(63)
      double precision pf321(63),pf322(63),pf323(63),pf324(63),pf325(63)
      double precision pf326(63),pf327(63),pf328(63),pf329(63),pf330(63)
      double precision pf331(63),pf332(63),pf333(63),pf334(63),pf335(63)
      double precision pf336(63),pf337(63),pf338(63),pf339(63),pf340(63)
      double precision pf341(63),pf342(63),pf343(63),pf344(63),pf345(63)
      double precision pf346(63),pf347(63),pf348(63),pf349(63),pf350(63)
      double precision pf351(63),pf352(63),pf353(63),pf354(63),pf355(63)
      double precision pf356(63),pf357(63),pf358(63),pf359(63),pf360(63)
      double precision pf361(63),pf362(63),pf363(63),pf364(63),pf365(63)
      double precision pf366(63),pf367(63),pf368(63),pf369(63),pf370(63)
      double precision pf371(63),pf372(63),pf373(63),pf374(63),pf375(63)
      double precision pf376(63),pf377(63),pf378(63),pf379(63),pf380(63)
      double precision pf381(63),pf382(63),pf383(63),pf384(63),pf385(63)
      double precision pf386(63),pf387(63),pf388(63),pf389(63),pf390(63)
      double precision pf391(63),pf392(63),pf393(63),pf394(63),pf395(63)
      double precision pf396(63),pf397(63),pf398(63),pf399(63),pf400(63)
      double precision pf401(63),pf402(63),pf403(63),pf404(63),pf405(63)
      double precision pf406(63),pf407(63),pf408(63),pf409(63),pf410(63)
      double precision pf411(63),pf412(63),pf413(63),pf414(63),pf415(63)
      double precision pf416(63),pf417(63),pf418(63),pf419(63),pf420(63)
      double precision pf421(63),pf422(63),pf423(63),pf424(63),pf425(63)
      double precision pf426(63),pf427(63),pf428(63),pf429(63),pf430(63)
      double precision pf431(63),pf432(63),pf433(63),pf434(63),pf435(63)
      double precision pf436(63),pf437(63),pf438(63),pf439(63),pf440(63)
      double precision pf441(63),pf442(63),pf443(63),pf444(63),pf445(63)
      double precision pf446(63),pf447(63),pf448(63),pf449(63),pf450(63)
      double precision pf451(63),pf452(63),pf453(63),pf454(63),pf455(63)
      double precision pf456(63),pf457(63),pf458(63),pf459(63),pf460(63)
      double precision pf461(63),pf462(63),pf463(63),pf464(63),pf465(63)
      double precision pf466(63),pf467(63),pf468(63),pf469(63),pf470(63)
      double precision pf471(63),pf472(63),pf473(63),pf474(63),pf475(63)
      double precision pf476(63),pf477(63),pf478(63),pf479(63),pf480(63)
      double precision pf481(63),pf482(63),pf483(63),pf484(63),pf485(63)
      double precision pf486(63),pf487(63),pf488(63),pf489(63),pf490(63)
      double precision pf491(63),pf492(63),pf493(63),pf494(63),pf495(63)
      double precision pf496(63),pf497(63),pf498(63),pf499(63),pf500(63)
      double precision pf501(63),pf502(63),pf503(63),pf504(63),pf505(63)
      double precision pf506(63),pf507(63),pf508(63),pf509(63),pf510(63)
      double precision pf511(63),pf512(63),pf513(63),pf514(63),pf515(63)
      double precision pf516(63),pf517(63),pf518(63),pf519(63),pf520(63)
      double precision pf521(63),pf522(63),pf523(63),pf524(63),pf525(63)
      double precision pf526(63),pf527(63),pf528(63),pf529(63),pf530(63)
      double precision pf531(63),pf532(63),pf533(63),pf534(63),pf535(63)
      double precision pf536(63),pf537(63),pf538(63),pf539(63),pf540(63)
      double precision pf541(63),pf542(63),pf543(63),pf544(63),pf545(63)
      double precision pf546(63),pf547(63),pf548(63),pf549(63),pf550(63)
      double precision pf551(63),pf552(63),pf553(63),pf554(63),pf555(63)
      double precision pf556(63),pf557(63),pf558(63),pf559(63),pf560(63)
      equivalence (pf001(1),p63(001,1)),(pf002(1),p63(1,002))
      equivalence (pf003(1),p63(1,003)),(pf004(1),p63(1,004))
      equivalence (pf005(1),p63(1,005)),(pf006(1),p63(1,006))
      equivalence (pf007(1),p63(1,007)),(pf008(1),p63(1,008))
      equivalence (pf009(1),p63(1,009)),(pf010(1),p63(1,010))
      equivalence (pf011(1),p63(1,011)),(pf012(1),p63(1,012))
      equivalence (pf013(1),p63(1,013)),(pf014(1),p63(1,014))
      equivalence (pf015(1),p63(1,015)),(pf016(1),p63(1,016))
      equivalence (pf017(1),p63(1,017)),(pf018(1),p63(1,018))
      equivalence (pf019(1),p63(1,019)),(pf020(1),p63(1,020))
      equivalence (pf021(1),p63(1,021)),(pf022(1),p63(1,022))
      equivalence (pf023(1),p63(1,023)),(pf024(1),p63(1,024))
      equivalence (pf025(1),p63(1,025)),(pf026(1),p63(1,026))
      equivalence (pf027(1),p63(1,027)),(pf028(1),p63(1,028))
      equivalence (pf029(1),p63(1,029)),(pf030(1),p63(1,030))
      equivalence (pf031(1),p63(1,031)),(pf032(1),p63(1,032))
      equivalence (pf033(1),p63(1,033)),(pf034(1),p63(1,034))
      equivalence (pf035(1),p63(1,035)),(pf036(1),p63(1,036))
      equivalence (pf037(1),p63(1,037)),(pf038(1),p63(1,038))
      equivalence (pf039(1),p63(1,039)),(pf040(1),p63(1,040))
      equivalence (pf041(1),p63(1,041)),(pf042(1),p63(1,042))
      equivalence (pf043(1),p63(1,043)),(pf044(1),p63(1,044))
      equivalence (pf045(1),p63(1,045)),(pf046(1),p63(1,046))
      equivalence (pf047(1),p63(1,047)),(pf048(1),p63(1,048))
      equivalence (pf049(1),p63(1,049)),(pf050(1),p63(1,050))
      equivalence (pf051(1),p63(1,051)),(pf052(1),p63(1,052))
      equivalence (pf053(1),p63(1,053)),(pf054(1),p63(1,054))
      equivalence (pf055(1),p63(1,055)),(pf056(1),p63(1,056))
      equivalence (pf057(1),p63(1,057)),(pf058(1),p63(1,058))
      equivalence (pf059(1),p63(1,059)),(pf060(1),p63(1,060))
      equivalence (pf061(1),p63(1,061)),(pf062(1),p63(1,062))
      equivalence (pf063(1),p63(1,063)),(pf064(1),p63(1,064))
      equivalence (pf065(1),p63(1,065)),(pf066(1),p63(1,066))
      equivalence (pf067(1),p63(1,067)),(pf068(1),p63(1,068))
      equivalence (pf069(1),p63(1,069)),(pf070(1),p63(1,070))
      equivalence (pf071(1),p63(1,071)),(pf072(1),p63(1,072))
      equivalence (pf073(1),p63(1,073)),(pf074(1),p63(1,074))
      equivalence (pf075(1),p63(1,075)),(pf076(1),p63(1,076))
      equivalence (pf077(1),p63(1,077)),(pf078(1),p63(1,078))
      equivalence (pf079(1),p63(1,079)),(pf080(1),p63(1,080))
      equivalence (pf081(1),p63(1,081)),(pf082(1),p63(1,082))
      equivalence (pf083(1),p63(1,083)),(pf084(1),p63(1,084))
      equivalence (pf085(1),p63(1,085)),(pf086(1),p63(1,086))
      equivalence (pf087(1),p63(1,087)),(pf088(1),p63(1,088))
      equivalence (pf089(1),p63(1,089)),(pf090(1),p63(1,090))
      equivalence (pf091(1),p63(1,091)),(pf092(1),p63(1,092))
      equivalence (pf093(1),p63(1,093)),(pf094(1),p63(1,094))
      equivalence (pf095(1),p63(1,095)),(pf096(1),p63(1,096))
      equivalence (pf097(1),p63(1,097)),(pf098(1),p63(1,098))
      equivalence (pf099(1),p63(1,099)),(pf100(1),p63(1,100))
      equivalence (pf101(1),p63(1,101)),(pf102(1),p63(1,102))
      equivalence (pf103(1),p63(1,103)),(pf104(1),p63(1,104))
      equivalence (pf105(1),p63(1,105)),(pf106(1),p63(1,106))
      equivalence (pf107(1),p63(1,107)),(pf108(1),p63(1,108))
      equivalence (pf109(1),p63(1,109)),(pf110(1),p63(1,110))
      equivalence (pf111(1),p63(1,111)),(pf112(1),p63(1,112))
      equivalence (pf113(1),p63(1,113)),(pf114(1),p63(1,114))
      equivalence (pf115(1),p63(1,115)),(pf116(1),p63(1,116))
      equivalence (pf117(1),p63(1,117)),(pf118(1),p63(1,118))
      equivalence (pf119(1),p63(1,119)),(pf120(1),p63(1,120))
      equivalence (pf121(1),p63(1,121)),(pf122(1),p63(1,122))
      equivalence (pf123(1),p63(1,123)),(pf124(1),p63(1,124))
      equivalence (pf125(1),p63(1,125)),(pf126(1),p63(1,126))
      equivalence (pf127(1),p63(1,127)),(pf128(1),p63(1,128))
      equivalence (pf129(1),p63(1,129)),(pf130(1),p63(1,130))
      equivalence (pf131(1),p63(1,131)),(pf132(1),p63(1,132))
      equivalence (pf133(1),p63(1,133)),(pf134(1),p63(1,134))
      equivalence (pf135(1),p63(1,135)),(pf136(1),p63(1,136))
      equivalence (pf137(1),p63(1,137)),(pf138(1),p63(1,138))
      equivalence (pf139(1),p63(1,139)),(pf140(1),p63(1,140))
      equivalence (pf141(1),p63(1,141)),(pf142(1),p63(1,142))
      equivalence (pf143(1),p63(1,143)),(pf144(1),p63(1,144))
      equivalence (pf145(1),p63(1,145)),(pf146(1),p63(1,146))
      equivalence (pf147(1),p63(1,147)),(pf148(1),p63(1,148))
      equivalence (pf149(1),p63(1,149)),(pf150(1),p63(1,150))
      equivalence (pf151(1),p63(1,151)),(pf152(1),p63(1,152))
      equivalence (pf153(1),p63(1,153)),(pf154(1),p63(1,154))
      equivalence (pf155(1),p63(1,155)),(pf156(1),p63(1,156))
      equivalence (pf157(1),p63(1,157)),(pf158(1),p63(1,158))
      equivalence (pf159(1),p63(1,159)),(pf160(1),p63(1,160))
      equivalence (pf161(1),p63(1,161)),(pf162(1),p63(1,162))
      equivalence (pf163(1),p63(1,163)),(pf164(1),p63(1,164))
      equivalence (pf165(1),p63(1,165)),(pf166(1),p63(1,166))
      equivalence (pf167(1),p63(1,167)),(pf168(1),p63(1,168))
      equivalence (pf169(1),p63(1,169)),(pf170(1),p63(1,170))
      equivalence (pf171(1),p63(1,171)),(pf172(1),p63(1,172))
      equivalence (pf173(1),p63(1,173)),(pf174(1),p63(1,174))
      equivalence (pf175(1),p63(1,175)),(pf176(1),p63(1,176))
      equivalence (pf177(1),p63(1,177)),(pf178(1),p63(1,178))
      equivalence (pf179(1),p63(1,179)),(pf180(1),p63(1,180))
      equivalence (pf181(1),p63(1,181)),(pf182(1),p63(1,182))
      equivalence (pf183(1),p63(1,183)),(pf184(1),p63(1,184))
      equivalence (pf185(1),p63(1,185)),(pf186(1),p63(1,186))
      equivalence (pf187(1),p63(1,187)),(pf188(1),p63(1,188))
      equivalence (pf189(1),p63(1,189)),(pf190(1),p63(1,190))
      equivalence (pf191(1),p63(1,191)),(pf192(1),p63(1,192))
      equivalence (pf193(1),p63(1,193)),(pf194(1),p63(1,194))
      equivalence (pf195(1),p63(1,195)),(pf196(1),p63(1,196))
      equivalence (pf197(1),p63(1,197)),(pf198(1),p63(1,198))
      equivalence (pf199(1),p63(1,199)),(pf200(1),p63(1,200))
      equivalence (pf201(1),p63(1,201)),(pf202(1),p63(1,202))
      equivalence (pf203(1),p63(1,203)),(pf204(1),p63(1,204))
      equivalence (pf205(1),p63(1,205)),(pf206(1),p63(1,206))
      equivalence (pf207(1),p63(1,207)),(pf208(1),p63(1,208))
      equivalence (pf209(1),p63(1,209)),(pf210(1),p63(1,210))
      equivalence (pf211(1),p63(1,211)),(pf212(1),p63(1,212))
      equivalence (pf213(1),p63(1,213)),(pf214(1),p63(1,214))
      equivalence (pf215(1),p63(1,215)),(pf216(1),p63(1,216))
      equivalence (pf217(1),p63(1,217)),(pf218(1),p63(1,218))
      equivalence (pf219(1),p63(1,219)),(pf220(1),p63(1,220))
      equivalence (pf221(1),p63(1,221)),(pf222(1),p63(1,222))
      equivalence (pf223(1),p63(1,223)),(pf224(1),p63(1,224))
      equivalence (pf225(1),p63(1,225)),(pf226(1),p63(1,226))
      equivalence (pf227(1),p63(1,227)),(pf228(1),p63(1,228))
      equivalence (pf229(1),p63(1,229)),(pf230(1),p63(1,230))
      equivalence (pf231(1),p63(1,231)),(pf232(1),p63(1,232))
      equivalence (pf233(1),p63(1,233)),(pf234(1),p63(1,234))
      equivalence (pf235(1),p63(1,235)),(pf236(1),p63(1,236))
      equivalence (pf237(1),p63(1,237)),(pf238(1),p63(1,238))
      equivalence (pf239(1),p63(1,239)),(pf240(1),p63(1,240))
      equivalence (pf241(1),p63(1,241)),(pf242(1),p63(1,242))
      equivalence (pf243(1),p63(1,243)),(pf244(1),p63(1,244))
      equivalence (pf245(1),p63(1,245)),(pf246(1),p63(1,246))
      equivalence (pf247(1),p63(1,247)),(pf248(1),p63(1,248))
      equivalence (pf249(1),p63(1,249)),(pf250(1),p63(1,250))
      equivalence (pf251(1),p63(1,251)),(pf252(1),p63(1,252))
      equivalence (pf253(1),p63(1,253)),(pf254(1),p63(1,254))
      equivalence (pf255(1),p63(1,255)),(pf256(1),p63(1,256))
      equivalence (pf257(1),p63(1,257)),(pf258(1),p63(1,258))
      equivalence (pf259(1),p63(1,259)),(pf260(1),p63(1,260))
      equivalence (pf261(1),p63(1,261)),(pf262(1),p63(1,262))
      equivalence (pf263(1),p63(1,263)),(pf264(1),p63(1,264))
      equivalence (pf265(1),p63(1,265)),(pf266(1),p63(1,266))
      equivalence (pf267(1),p63(1,267)),(pf268(1),p63(1,268))
      equivalence (pf269(1),p63(1,269)),(pf270(1),p63(1,270))
      equivalence (pf271(1),p63(1,271)),(pf272(1),p63(1,272))
      equivalence (pf273(1),p63(1,273)),(pf274(1),p63(1,274))
      equivalence (pf275(1),p63(1,275)),(pf276(1),p63(1,276))
      equivalence (pf277(1),p63(1,277)),(pf278(1),p63(1,278))
      equivalence (pf279(1),p63(1,279)),(pf280(1),p63(1,280))
      equivalence (pf281(1),p63(1,281)),(pf282(1),p63(1,282))
      equivalence (pf283(1),p63(1,283)),(pf284(1),p63(1,284))
      equivalence (pf285(1),p63(1,285)),(pf286(1),p63(1,286))
      equivalence (pf287(1),p63(1,287)),(pf288(1),p63(1,288))
      equivalence (pf289(1),p63(1,289)),(pf290(1),p63(1,290))
      equivalence (pf291(1),p63(1,291)),(pf292(1),p63(1,292))
      equivalence (pf293(1),p63(1,293)),(pf294(1),p63(1,294))
      equivalence (pf295(1),p63(1,295)),(pf296(1),p63(1,296))
      equivalence (pf297(1),p63(1,297)),(pf298(1),p63(1,298))
      equivalence (pf299(1),p63(1,299)),(pf300(1),p63(1,300))
      equivalence (pf301(1),p63(1,301)),(pf302(1),p63(1,302))
      equivalence (pf303(1),p63(1,303)),(pf304(1),p63(1,304))
      equivalence (pf305(1),p63(1,305)),(pf306(1),p63(1,306))
      equivalence (pf307(1),p63(1,307)),(pf308(1),p63(1,308))
      equivalence (pf309(1),p63(1,309)),(pf310(1),p63(1,310))
      equivalence (pf311(1),p63(1,311)),(pf312(1),p63(1,312))
      equivalence (pf313(1),p63(1,313)),(pf314(1),p63(1,314))
      equivalence (pf315(1),p63(1,315)),(pf316(1),p63(1,316))
      equivalence (pf317(1),p63(1,317)),(pf318(1),p63(1,318))
      equivalence (pf319(1),p63(1,319)),(pf320(1),p63(1,320))
      equivalence (pf321(1),p63(1,321)),(pf322(1),p63(1,322))
      equivalence (pf323(1),p63(1,323)),(pf324(1),p63(1,324))
      equivalence (pf325(1),p63(1,325)),(pf326(1),p63(1,326))
      equivalence (pf327(1),p63(1,327)),(pf328(1),p63(1,328))
      equivalence (pf329(1),p63(1,329)),(pf330(1),p63(1,330))
      equivalence (pf331(1),p63(1,331)),(pf332(1),p63(1,332))
      equivalence (pf333(1),p63(1,333)),(pf334(1),p63(1,334))
      equivalence (pf335(1),p63(1,335)),(pf336(1),p63(1,336))
      equivalence (pf337(1),p63(1,337)),(pf338(1),p63(1,338))
      equivalence (pf339(1),p63(1,339)),(pf340(1),p63(1,340))
      equivalence (pf341(1),p63(1,341)),(pf342(1),p63(1,342))
      equivalence (pf343(1),p63(1,343)),(pf344(1),p63(1,344))
      equivalence (pf345(1),p63(1,345)),(pf346(1),p63(1,346))
      equivalence (pf347(1),p63(1,347)),(pf348(1),p63(1,348))
      equivalence (pf349(1),p63(1,349)),(pf350(1),p63(1,350))
      equivalence (pf351(1),p63(1,351)),(pf352(1),p63(1,352))
      equivalence (pf353(1),p63(1,353)),(pf354(1),p63(1,354))
      equivalence (pf355(1),p63(1,355)),(pf356(1),p63(1,356))
      equivalence (pf357(1),p63(1,357)),(pf358(1),p63(1,358))
      equivalence (pf359(1),p63(1,359)),(pf360(1),p63(1,360))
      equivalence (pf361(1),p63(1,361)),(pf362(1),p63(1,362))
      equivalence (pf363(1),p63(1,363)),(pf364(1),p63(1,364))
      equivalence (pf365(1),p63(1,365)),(pf366(1),p63(1,366))
      equivalence (pf367(1),p63(1,367)),(pf368(1),p63(1,368))
      equivalence (pf369(1),p63(1,369)),(pf370(1),p63(1,370))
      equivalence (pf371(1),p63(1,371)),(pf372(1),p63(1,372))
      equivalence (pf373(1),p63(1,373)),(pf374(1),p63(1,374))
      equivalence (pf375(1),p63(1,375)),(pf376(1),p63(1,376))
      equivalence (pf377(1),p63(1,377)),(pf378(1),p63(1,378))
      equivalence (pf379(1),p63(1,379)),(pf380(1),p63(1,380))
      equivalence (pf381(1),p63(1,381)),(pf382(1),p63(1,382))
      equivalence (pf383(1),p63(1,383)),(pf384(1),p63(1,384))
      equivalence (pf385(1),p63(1,385)),(pf386(1),p63(1,386))
      equivalence (pf387(1),p63(1,387)),(pf388(1),p63(1,388))
      equivalence (pf389(1),p63(1,389)),(pf390(1),p63(1,390))
      equivalence (pf391(1),p63(1,391)),(pf392(1),p63(1,392))
      equivalence (pf393(1),p63(1,393)),(pf394(1),p63(1,394))
      equivalence (pf395(1),p63(1,395)),(pf396(1),p63(1,396))
      equivalence (pf397(1),p63(1,397)),(pf398(1),p63(1,398))
      equivalence (pf399(1),p63(1,399)),(pf400(1),p63(1,400))
      equivalence (pf401(1),p63(1,401)),(pf402(1),p63(1,402))
      equivalence (pf403(1),p63(1,403)),(pf404(1),p63(1,404))
      equivalence (pf405(1),p63(1,405)),(pf406(1),p63(1,406))
      equivalence (pf407(1),p63(1,407)),(pf408(1),p63(1,408))
      equivalence (pf409(1),p63(1,409)),(pf410(1),p63(1,410))
      equivalence (pf411(1),p63(1,411)),(pf412(1),p63(1,412))
      equivalence (pf413(1),p63(1,413)),(pf414(1),p63(1,414))
      equivalence (pf415(1),p63(1,415)),(pf416(1),p63(1,416))
      equivalence (pf417(1),p63(1,417)),(pf418(1),p63(1,418))
      equivalence (pf419(1),p63(1,419)),(pf420(1),p63(1,420))
      equivalence (pf421(1),p63(1,421)),(pf422(1),p63(1,422))
      equivalence (pf423(1),p63(1,423)),(pf424(1),p63(1,424))
      equivalence (pf425(1),p63(1,425)),(pf426(1),p63(1,426))
      equivalence (pf427(1),p63(1,427)),(pf428(1),p63(1,428))
      equivalence (pf429(1),p63(1,429)),(pf430(1),p63(1,430))
      equivalence (pf431(1),p63(1,431)),(pf432(1),p63(1,432))
      equivalence (pf433(1),p63(1,433)),(pf434(1),p63(1,434))
      equivalence (pf435(1),p63(1,435)),(pf436(1),p63(1,436))
      equivalence (pf437(1),p63(1,437)),(pf438(1),p63(1,438))
      equivalence (pf439(1),p63(1,439)),(pf440(1),p63(1,440))
      equivalence (pf441(1),p63(1,441)),(pf442(1),p63(1,442))
      equivalence (pf443(1),p63(1,443)),(pf444(1),p63(1,444))
      equivalence (pf445(1),p63(1,445)),(pf446(1),p63(1,446))
      equivalence (pf447(1),p63(1,447)),(pf448(1),p63(1,448))
      equivalence (pf449(1),p63(1,449)),(pf450(1),p63(1,450))
      equivalence (pf451(1),p63(1,451)),(pf452(1),p63(1,452))
      equivalence (pf453(1),p63(1,453)),(pf454(1),p63(1,454))
      equivalence (pf455(1),p63(1,455)),(pf456(1),p63(1,456))
      equivalence (pf457(1),p63(1,457)),(pf458(1),p63(1,458))
      equivalence (pf459(1),p63(1,459)),(pf460(1),p63(1,460))
      equivalence (pf461(1),p63(1,461)),(pf462(1),p63(1,462))
      equivalence (pf463(1),p63(1,463)),(pf464(1),p63(1,464))
      equivalence (pf465(1),p63(1,465)),(pf466(1),p63(1,466))
      equivalence (pf467(1),p63(1,467)),(pf468(1),p63(1,468))
      equivalence (pf469(1),p63(1,469)),(pf470(1),p63(1,470))
      equivalence (pf471(1),p63(1,471)),(pf472(1),p63(1,472))
      equivalence (pf473(1),p63(1,473)),(pf474(1),p63(1,474))
      equivalence (pf475(1),p63(1,475)),(pf476(1),p63(1,476))
      equivalence (pf477(1),p63(1,477)),(pf478(1),p63(1,478))
      equivalence (pf479(1),p63(1,479)),(pf480(1),p63(1,480))
      equivalence (pf481(1),p63(1,481)),(pf482(1),p63(1,482))
      equivalence (pf483(1),p63(1,483)),(pf484(1),p63(1,484))
      equivalence (pf485(1),p63(1,485)),(pf486(1),p63(1,486))
      equivalence (pf487(1),p63(1,487)),(pf488(1),p63(1,488))
      equivalence (pf489(1),p63(1,489)),(pf490(1),p63(1,490))
      equivalence (pf491(1),p63(1,491)),(pf492(1),p63(1,492))
      equivalence (pf493(1),p63(1,493)),(pf494(1),p63(1,494))
      equivalence (pf495(1),p63(1,495)),(pf496(1),p63(1,496))
      equivalence (pf497(1),p63(1,497)),(pf498(1),p63(1,498))
      equivalence (pf499(1),p63(1,499)),(pf500(1),p63(1,500))
      equivalence (pf501(1),p63(1,501)),(pf502(1),p63(1,502))
      equivalence (pf503(1),p63(1,503)),(pf504(1),p63(1,504))
      equivalence (pf505(1),p63(1,505)),(pf506(1),p63(1,506))
      equivalence (pf507(1),p63(1,507)),(pf508(1),p63(1,508))
      equivalence (pf509(1),p63(1,509)),(pf510(1),p63(1,510))
      equivalence (pf511(1),p63(1,511)),(pf512(1),p63(1,512))
      equivalence (pf513(1),p63(1,513)),(pf514(1),p63(1,514))
      equivalence (pf515(1),p63(1,515)),(pf516(1),p63(1,516))
      equivalence (pf517(1),p63(1,517)),(pf518(1),p63(1,518))
      equivalence (pf519(1),p63(1,519)),(pf520(1),p63(1,520))
      equivalence (pf521(1),p63(1,521)),(pf522(1),p63(1,522))
      equivalence (pf523(1),p63(1,523)),(pf524(1),p63(1,524))
      equivalence (pf525(1),p63(1,525)),(pf526(1),p63(1,526))
      equivalence (pf527(1),p63(1,527)),(pf528(1),p63(1,528))
      equivalence (pf529(1),p63(1,529)),(pf530(1),p63(1,530))
      equivalence (pf531(1),p63(1,531)),(pf532(1),p63(1,532))
      equivalence (pf533(1),p63(1,533)),(pf534(1),p63(1,534))
      equivalence (pf535(1),p63(1,535)),(pf536(1),p63(1,536))
      equivalence (pf537(1),p63(1,537)),(pf538(1),p63(1,538))
      equivalence (pf539(1),p63(1,539)),(pf540(1),p63(1,540))
      equivalence (pf541(1),p63(1,541)),(pf542(1),p63(1,542))
      equivalence (pf543(1),p63(1,543)),(pf544(1),p63(1,544))
      equivalence (pf545(1),p63(1,545)),(pf546(1),p63(1,546))
      equivalence (pf547(1),p63(1,547)),(pf548(1),p63(1,548))
      equivalence (pf549(1),p63(1,549)),(pf550(1),p63(1,550))
      equivalence (pf551(1),p63(1,551)),(pf552(1),p63(1,552))
      equivalence (pf553(1),p63(1,553)),(pf554(1),p63(1,554))
      equivalence (pf555(1),p63(1,555)),(pf556(1),p63(1,556))
      equivalence (pf557(1),p63(1,557)),(pf558(1),p63(1,558))
      equivalence (pf559(1),p63(1,559)),(pf560(1),p63(1,560))
      data pf001/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     4    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     5    1.002,    1.002,   1.002,   1.002,   1.002,   1.002,   1.001,
     6    1.002,    1.002,   1.002,   1.002,   1.002,   1.002,   1.002,
     7    1.004,    1.004,   1.004,   1.004,   1.004,   1.004,   1.003,
     8    1.005,    1.005,   1.005,   1.005,   1.005,   1.005,   1.004,
     9    1.007,    1.007,   1.007,   1.007,   1.007,   1.007,   1.006/
      data pf002/
     1    1.010,    1.010,   1.010,   1.010,   1.010,   1.010,   1.009,
     2    1.015,    1.015,   1.015,   1.015,   1.015,   1.015,   1.012,
     3    1.020,    1.020,   1.020,   1.020,   1.020,   1.020,   1.016,
     4    1.027,    1.027,   1.027,   1.027,   1.027,   1.027,   1.021,
     5    1.036,    1.036,   1.036,   1.036,   1.036,   1.036,   1.028,
     6    1.048,    1.048,   1.048,   1.048,   1.048,   1.048,   1.036,
     7    1.064,    1.064,   1.064,   1.064,   1.064,   1.063,   1.046,
     8    1.083,    1.083,   1.083,   1.083,   1.083,   1.082,   1.059,
     9    1.107,    1.107,   1.107,   1.107,   1.106,   1.106,   1.074/
      data pf003/
     1    1.137,    1.137,   1.136,   1.136,   1.136,   1.134,   1.092,
     2    1.173,    1.173,   1.173,   1.172,   1.172,   1.169,   1.113,
     3    1.245,    1.244,   1.243,   1.242,   1.240,   1.235,   1.151,
     4    1.341,    1.340,   1.336,   1.333,   1.330,   1.321,   1.198,
     5    1.470,    1.467,   1.460,   1.454,   1.447,   1.430,   1.256,
     6    1.643,    1.638,   1.623,   1.610,   1.596,   1.566,   1.324,
     7    1.877,    1.866,   1.837,   1.810,   1.785,   1.733,   1.405,
     8    2.193,    2.172,   2.117,   2.067,   2.022,   1.934,   1.498,
     9    2.623,    2.584,   2.484,   2.393,   2.314,   2.174,   1.604/
      data pf004/
     1    3.208,    3.139,   2.963,   2.805,   2.673,   2.455,   1.723,
     2    4.003,    3.886,   3.586,   3.323,   3.109,   2.780,   1.856,
     3    5.079,    4.886,   4.396,   3.969,   3.634,   3.151,   2.001,
     4    7.740,    7.327,   6.292,   5.407,   4.743,   3.874,   2.271,
     5   11.943,   11.130,   9.114,   7.417,   6.196,   4.732,   2.573,
     6   18.324,   16.837,  13.185,  10.154,   8.052,   5.718,   2.901,
     7   27.604,   25.059,  18.856,  13.773,  10.361,   6.824,   3.252,
     8   40.529,   36.419,  26.474,  18.417,  13.161,   8.037,   3.618,
     9   57.791,   51.489,  36.344,  24.194,  16.470,   9.338,   3.994/
      data pf005/
     1   79.950,   70.728,  48.693,  31.173,  20.285,  10.708,   4.375,
     2  107.373,   94.424,  63.648,  39.369,  24.584,  12.126,   4.755,
     3  140.190,  122.667,  81.214,  48.743,  29.320,  13.570,   5.129,
     4  178.280,  155.336, 101.281,  59.203,  34.433,  15.022,   5.494,
     5  221.284,  192.109, 123.627,  70.616,  39.851,  16.463,   5.847,
     6  268.640,  232.499, 147.943,  82.815,  45.493,  17.877,   6.184,
     7  319.634,  275.893, 173.857,  95.612,  51.276,  19.251,   6.505,
     8  373.448,  321.597, 200.957, 108.812,  57.120,  20.573,   6.807,
     9  429.222,  368.884, 228.823, 122.221,  62.948,  21.836,   7.090/
      data pf006/
     1  486.101,  417.035, 257.045, 135.655,  68.694,  23.033,   7.355,
     2  543.273,  465.369, 285.238, 148.949,  74.299,  24.160,   7.600,
     3  600.000,  513.270, 313.063, 161.960,  79.715,  25.215,   7.826,
     4  655.640,  560.204, 340.224, 174.566,  84.903,  26.198,   8.034,
     5  709.651,  605.723, 366.480, 186.673,  89.835,  27.109,   8.226,
     6  761.599,  649.468, 391.641, 198.207,  94.492,  27.949,   8.400,
     7  811.155,  691.168, 415.563, 209.116,  98.862,  28.722,   8.559,
     8  858.081,  730.629, 438.150, 219.370, 102.941,  29.431,   8.704,
     9  902.226,  767.732, 459.345, 228.952, 106.728,  30.078,   8.835/
      data pf007/
     1  943.513,  802.414, 479.121, 237.861, 110.229,  30.668,   8.954,
     2  981.926,  834.668, 497.484, 246.106, 113.453,  31.205,   9.062,
     3    2.001,    2.001,   2.001,   2.001,   2.001,   2.001,   2.001,
     4    2.001,    2.001,   2.001,   2.001,   2.001,   2.001,   2.001,
     5    2.002,    2.002,   2.002,   2.002,   2.002,   2.002,   2.002,
     6    2.003,    2.003,   2.003,   2.003,   2.003,   2.003,   2.003,
     7    2.004,    2.004,   2.004,   2.004,   2.004,   2.004,   2.004,
     8    2.006,    2.006,   2.006,   2.006,   2.006,   2.006,   2.006,
     9    2.008,    2.008,   2.008,   2.008,   2.008,   2.008,   2.008/
      data pf008/
     1    2.011,    2.011,   2.011,   2.011,   2.011,   2.011,   2.011,
     2    2.015,    2.015,   2.015,   2.015,   2.015,   2.015,   2.015,
     3    2.020,    2.020,   2.020,   2.020,   2.020,   2.020,   2.020,
     4    2.026,    2.026,   2.026,   2.026,   2.026,   2.026,   2.026,
     5    2.034,    2.034,   2.034,   2.034,   2.034,   2.034,   2.034,
     6    2.044,    2.044,   2.044,   2.044,   2.044,   2.044,   2.044,
     7    2.057,    2.057,   2.057,   2.057,   2.057,   2.057,   2.057,
     8    2.072,    2.072,   2.072,   2.072,   2.072,   2.072,   2.072,
     9    2.090,    2.090,   2.090,   2.090,   2.090,   2.090,   2.090/
      data pf009/
     1    2.111,    2.111,   2.111,   2.111,   2.111,   2.111,   2.111,
     2    2.137,    2.137,   2.137,   2.137,   2.137,   2.137,   2.137,
     3    2.166,    2.166,   2.166,   2.166,   2.166,   2.166,   2.166,
     4    2.201,    2.201,   2.201,   2.201,   2.201,   2.201,   2.201,
     5    2.262,    2.262,   2.262,   2.262,   2.262,   2.262,   2.262,
     6    2.337,    2.337,   2.337,   2.337,   2.337,   2.337,   2.337,
     7    2.426,    2.426,   2.426,   2.426,   2.426,   2.426,   2.426,
     8    2.532,    2.532,   2.532,   2.532,   2.532,   2.532,   2.532,
     9    2.654,    2.654,   2.654,   2.654,   2.654,   2.654,   2.654/
      data pf010/
     1    2.795,    2.795,   2.795,   2.795,   2.795,   2.795,   2.795,
     2    2.956,    2.956,   2.956,   2.956,   2.955,   2.955,   2.955,
     3    3.136,    3.136,   3.136,   3.136,   3.136,   3.136,   3.135,
     4    3.337,    3.337,   3.337,   3.337,   3.337,   3.337,   3.336,
     5    3.561,    3.561,   3.561,   3.561,   3.560,   3.559,   3.557,
     6    3.986,    3.986,   3.986,   3.985,   3.982,   3.979,   3.974,
     7    4.487,    4.487,   4.487,   4.486,   4.476,   4.465,   4.450,
     8    5.091,    5.091,   5.090,   5.087,   5.055,   5.024,   4.988,
     9    5.845,    5.845,   5.843,   5.835,   5.749,   5.668,   5.587/
      data pf011/
     1    6.839,    6.839,   6.834,   6.812,   6.603,   6.418,   6.251,
     2    8.210,    8.210,   8.199,   8.150,   7.690,   7.298,   6.981,
     3   10.161,   10.161,  10.137,  10.036,   9.104,   8.342,   7.778,
     4   12.952,   12.952,  12.906,  12.712,  10.968,   9.588,   8.644,
     5   16.896,   16.896,  16.813,  16.467,  13.414,  11.073,   9.579,
     6   22.330,   22.330,  22.190,  21.610,  16.582,  12.830,  10.579,
     7   29.582,   29.582,  29.360,  28.441,  20.599,  14.885,  11.641,
     8   38.941,   38.941,  38.605,  37.219,  25.564,  17.251,  12.756,
     9   50.615,   50.615,  50.129,  48.131,  31.540,  19.929,  13.915/
      data pf012/
     1   64.715,   64.715,  64.040,  61.273,  38.543,  22.903,  15.106,
     2   81.239,   81.239,  80.334,  76.633,  46.543,  26.147,  16.318,
     3  100.071,  100.071,  98.896,  94.102,  55.463,  29.620,  17.537,
     4  120.997,  120.997, 119.513, 113.476,  65.190,  33.278,  18.750,
     5  143.719,  143.719, 141.894, 134.478,  75.583,  37.068,  19.947,
     6  167.885,  167.885, 165.689, 156.781,  86.480,  40.937,  21.115,
     7  193.110,  193.110, 190.520, 180.032,  97.715,  44.833,  22.246,
     8  219.001,  219.001, 216.001, 203.870, 109.124,  48.709,  23.333,
     9  245.178,  245.178, 241.759, 227.946, 120.549,  52.521,  24.369/
      data pf013/
     1  271.290,  271.290, 267.447, 251.942, 131.852,  56.232,  25.351,
     2  297.023,  297.023, 292.759, 275.570, 142.909,  59.812,  26.274,
     3  322.110,  322.110, 317.432, 298.590, 153.619,  63.237,  27.139,
     4  346.332,  346.332, 341.251, 320.802, 163.900,  66.488,  27.944,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf014/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf015/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf016/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf017/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     8    1.003,    1.003,   1.003,   1.003,   1.003,   1.003,   1.003,
     9    1.010,    1.010,   1.010,   1.010,   1.010,   1.010,   1.010/
      data pf018/
     1    1.029,    1.029,   1.029,   1.029,   1.029,   1.028,   1.027,
     2    1.076,    1.076,   1.076,   1.076,   1.076,   1.072,   1.068,
     3    1.189,    1.189,   1.189,   1.189,   1.189,   1.171,   1.156,
     4    1.448,    1.448,   1.448,   1.448,   1.448,   1.386,   1.336,
     5    2.015,    2.015,   2.015,   2.015,   2.015,   1.829,   1.679,
     6    3.192,    3.192,   3.192,   3.192,   3.192,   2.692,   2.302,
     7    5.496,    5.496,   5.496,   5.496,   5.495,   4.291,   3.371,
     8    9.727,    9.727,   9.727,   9.727,   9.726,   7.090,   5.113,
     9   17.025,   17.025,  17.025,  17.025,  17.022,  11.723,   7.814/
      data pf019/
     1   28.861,   28.861,  28.861,  28.861,  28.855,  18.980,  11.805,
     2   46.977,   46.977,  46.977,  46.977,  46.967,  29.767,  17.438,
     3   73.250,   73.250,  73.250,  73.250,  73.233,  45.030,  25.055,
     4  109.508,  109.508, 109.508, 109.508, 109.482,  65.657,  34.949,
     5  157.338,  157.338, 157.338, 157.338, 157.299,  92.388,  47.334,
     6  217.901,  217.901, 217.901, 217.901, 217.847, 125.722,  62.321,
     7    4.234,    4.234,   4.234,   4.234,   4.234,   4.234,   4.234,
     8    4.257,    4.257,   4.257,   4.257,   4.257,   4.257,   4.257,
     9    4.282,    4.282,   4.282,   4.282,   4.282,   4.282,   4.282/
      data pf020/
     1    4.308,    4.308,   4.308,   4.308,   4.308,   4.308,   4.308,
     2    4.335,    4.335,   4.335,   4.335,   4.335,   4.335,   4.335,
     3    4.363,    4.363,   4.363,   4.363,   4.363,   4.363,   4.363,
     4    4.392,    4.392,   4.392,   4.392,   4.392,   4.392,   4.392,
     5    4.422,    4.422,   4.422,   4.422,   4.422,   4.422,   4.422,
     6    4.453,    4.453,   4.453,   4.453,   4.453,   4.453,   4.453,
     7    4.484,    4.484,   4.484,   4.484,   4.484,   4.484,   4.484,
     8    4.516,    4.516,   4.516,   4.516,   4.516,   4.516,   4.516,
     9    4.548,    4.548,   4.548,   4.548,   4.548,   4.548,   4.548/
      data pf021/
     1    4.581,    4.581,   4.581,   4.581,   4.581,   4.581,   4.581,
     2    4.615,    4.615,   4.615,   4.615,   4.615,   4.615,   4.615,
     3    4.648,    4.648,   4.648,   4.648,   4.648,   4.648,   4.648,
     4    4.682,    4.682,   4.682,   4.682,   4.682,   4.682,   4.682,
     5    4.716,    4.716,   4.716,   4.716,   4.716,   4.716,   4.716,
     6    4.749,    4.749,   4.749,   4.749,   4.749,   4.749,   4.749,
     7    4.783,    4.783,   4.783,   4.783,   4.783,   4.783,   4.783,
     8    4.817,    4.817,   4.817,   4.817,   4.817,   4.817,   4.817,
     9    4.867,    4.867,   4.867,   4.867,   4.867,   4.867,   4.867/
      data pf022/
     1    4.917,    4.917,   4.917,   4.917,   4.917,   4.917,   4.917,
     2    4.966,    4.966,   4.966,   4.966,   4.966,   4.966,   4.966,
     3    5.014,    5.014,   5.014,   5.014,   5.014,   5.014,   5.014,
     4    5.061,    5.061,   5.061,   5.061,   5.061,   5.061,   5.061,
     5    5.107,    5.107,   5.107,   5.107,   5.107,   5.107,   5.107,
     6    5.152,    5.152,   5.152,   5.152,   5.152,   5.152,   5.152,
     7    5.195,    5.195,   5.195,   5.195,   5.195,   5.195,   5.195,
     8    5.237,    5.237,   5.237,   5.237,   5.237,   5.237,   5.237,
     9    5.277,    5.277,   5.277,   5.277,   5.277,   5.277,   5.277/
      data pf023/
     1    5.341,    5.341,   5.341,   5.341,   5.341,   5.341,   5.341,
     2    5.400,    5.400,   5.400,   5.400,   5.400,   5.400,   5.400,
     3    5.456,    5.456,   5.456,   5.456,   5.456,   5.456,   5.456,
     4    5.507,    5.507,   5.507,   5.507,   5.507,   5.507,   5.507,
     5    5.554,    5.554,   5.554,   5.554,   5.554,   5.554,   5.554,
     6    5.597,    5.597,   5.597,   5.597,   5.597,   5.597,   5.597,
     7    5.637,    5.637,   5.637,   5.637,   5.637,   5.637,   5.637,
     8    5.674,    5.674,   5.674,   5.674,   5.674,   5.674,   5.674,
     9    5.708,    5.708,   5.708,   5.708,   5.708,   5.708,   5.708/
      data pf024/
     1    5.743,    5.743,   5.743,   5.743,   5.743,   5.743,   5.743,
     2    5.782,    5.782,   5.782,   5.782,   5.782,   5.782,   5.782,
     3    5.837,    5.837,   5.837,   5.837,   5.837,   5.837,   5.837,
     4    5.926,    5.926,   5.926,   5.926,   5.926,   5.926,   5.926,
     5    6.084,    6.084,   6.084,   6.084,   6.084,   6.083,   6.083,
     6    6.374,    6.374,   6.374,   6.374,   6.373,   6.372,   6.368,
     7    6.909,    6.909,   6.909,   6.908,   6.906,   6.902,   6.884,
     8    7.897,    7.897,   7.896,   7.895,   7.886,   7.872,   7.809,
     9    9.718,    9.718,   9.717,   9.712,   9.683,   9.637,   9.440/
      data pf025/
     1   13.044,   13.044,  13.041,  13.027,  12.942,  12.813,  12.266,
     2   18.991,   18.991,  18.982,  18.946,  18.727,  18.400,  17.045,
     3   29.280,   29.280,  29.258,  29.174,  28.669,  27.918,  24.875,
     4   46.353,   46.353,  46.308,  46.128,  45.061,  43.487,  37.230,
     5   73.396,   73.396,  73.306,  72.954,  70.877,  67.832,  55.938,
     6  114.209,  114.209, 114.046, 113.406, 109.642, 104.162,  83.077,
     7  172.942,  172.942, 172.665, 171.574, 165.183, 155.929, 120.806,
     8  253.710,  253.710, 253.264, 251.510, 241.265, 226.503, 171.153,
     9    5.678,    5.678,   5.678,   5.678,   5.678,   5.678,   5.678/
      data pf026/
     1    5.733,    5.733,   5.733,   5.733,   5.733,   5.733,   5.733,
     2    5.790,    5.790,   5.790,   5.790,   5.790,   5.790,   5.790,
     3    5.849,    5.849,   5.849,   5.849,   5.849,   5.849,   5.849,
     4    5.910,    5.910,   5.910,   5.910,   5.910,   5.910,   5.910,
     5    5.972,    5.972,   5.972,   5.972,   5.972,   5.972,   5.972,
     6    6.035,    6.035,   6.035,   6.035,   6.035,   6.035,   6.035,
     7    6.099,    6.099,   6.099,   6.099,   6.099,   6.099,   6.099,
     8    6.165,    6.165,   6.165,   6.165,   6.165,   6.165,   6.165,
     9    6.231,    6.231,   6.231,   6.231,   6.231,   6.231,   6.231/
      data pf027/
     1    6.298,    6.298,   6.298,   6.298,   6.298,   6.298,   6.298,
     2    6.365,    6.365,   6.365,   6.365,   6.365,   6.365,   6.365,
     3    6.433,    6.433,   6.433,   6.433,   6.433,   6.433,   6.433,
     4    6.501,    6.501,   6.501,   6.501,   6.501,   6.501,   6.501,
     5    6.570,    6.570,   6.570,   6.570,   6.570,   6.570,   6.570,
     6    6.639,    6.639,   6.639,   6.639,   6.639,   6.639,   6.639,
     7    6.708,    6.708,   6.708,   6.708,   6.708,   6.708,   6.708,
     8    6.777,    6.777,   6.777,   6.777,   6.777,   6.777,   6.777,
     9    6.847,    6.847,   6.847,   6.847,   6.847,   6.847,   6.847/
      data pf028/
     1    6.917,    6.917,   6.917,   6.917,   6.917,   6.917,   6.917,
     2    7.023,    7.023,   7.023,   7.023,   7.023,   7.023,   7.023,
     3    7.130,    7.130,   7.130,   7.130,   7.130,   7.130,   7.130,
     4    7.239,    7.239,   7.239,   7.239,   7.239,   7.239,   7.239,
     5    7.350,    7.350,   7.350,   7.350,   7.350,   7.350,   7.350,
     6    7.463,    7.463,   7.463,   7.463,   7.463,   7.463,   7.463,
     7    7.579,    7.579,   7.579,   7.579,   7.579,   7.579,   7.579,
     8    7.699,    7.699,   7.699,   7.699,   7.699,   7.699,   7.699,
     9    7.822,    7.822,   7.822,   7.822,   7.822,   7.822,   7.822/
      data pf029/
     1    7.950,    7.950,   7.950,   7.950,   7.950,   7.950,   7.950,
     2    8.082,    8.082,   8.082,   8.082,   8.082,   8.082,   8.082,
     3    8.312,    8.312,   8.312,   8.312,   8.312,   8.312,   8.312,
     4    8.555,    8.555,   8.555,   8.555,   8.555,   8.555,   8.555,
     5    8.811,    8.811,   8.811,   8.811,   8.811,   8.811,   8.811,
     6    9.078,    9.078,   9.078,   9.078,   9.078,   9.078,   9.078,
     7    9.355,    9.355,   9.355,   9.355,   9.355,   9.355,   9.355,
     8    9.641,    9.641,   9.641,   9.641,   9.641,   9.641,   9.641,
     9    9.932,    9.932,   9.932,   9.932,   9.932,   9.932,   9.932/
      data pf030/
     1   10.226,   10.226,  10.226,  10.226,  10.226,  10.226,  10.226,
     2   10.523,   10.523,  10.523,  10.523,  10.523,  10.523,  10.523,
     3   10.821,   10.821,  10.821,  10.821,  10.821,  10.821,  10.821,
     4   11.125,   11.125,  11.125,  11.125,  11.125,  11.125,  11.125,
     5   11.443,   11.443,  11.443,  11.443,  11.443,  11.443,  11.443,
     6   11.792,   11.792,  11.792,  11.792,  11.792,  11.792,  11.792,
     7   12.207,   12.207,  12.207,  12.207,  12.207,  12.207,  12.207,
     8   12.740,   12.740,  12.740,  12.740,  12.740,  12.740,  12.740,
     9   13.484,   13.484,  13.484,  13.484,  13.484,  13.484,  13.483/
      data pf031/
     1   14.582,   14.582,  14.582,  14.582,  14.582,  14.581,  14.577,
     2   16.277,   16.276,  16.275,  16.275,  16.274,  16.269,  16.252,
     3   18.975,   18.972,  18.968,  18.967,  18.963,  18.945,  18.883,
     4   23.372,   23.363,  23.347,  23.345,  23.332,  23.274,  23.076,
     5   30.624,   30.598,  30.553,  30.545,  30.508,  30.342,  29.788,
     6   42.569,   42.500,  42.384,  42.364,  42.268,  41.846,  40.458,
     7   61.936,   61.776,  61.506,  61.459,  61.235,  60.265,  57.122,
     8   92.494,   92.154,  91.580,  91.481,  91.008,  88.967,  82.453,
     9  139.043,  138.378, 137.254, 137.060, 136.138, 132.180, 119.707/
      data pf032/
     1  207.208,  205.997, 203.952, 203.601, 201.928, 194.787, 172.533,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf033/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     8    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     9    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001/
      data pf034/
     1    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     2    4.003,    4.003,   4.003,   4.003,   4.003,   4.003,   4.003,
     3    4.004,    4.004,   4.004,   4.004,   4.004,   4.004,   4.004,
     4    4.007,    4.007,   4.007,   4.007,   4.007,   4.007,   4.007,
     5    4.012,    4.012,   4.012,   4.012,   4.012,   4.012,   4.012,
     6    4.018,    4.018,   4.018,   4.018,   4.018,   4.018,   4.018,
     7    4.028,    4.028,   4.028,   4.028,   4.028,   4.028,   4.028,
     8    4.041,    4.041,   4.041,   4.041,   4.041,   4.041,   4.041,
     9    4.060,    4.060,   4.060,   4.060,   4.060,   4.060,   4.060/
      data pf035/
     1    4.085,    4.085,   4.085,   4.085,   4.085,   4.085,   4.085,
     2    4.118,    4.118,   4.118,   4.118,   4.118,   4.118,   4.118,
     3    4.160,    4.160,   4.160,   4.160,   4.160,   4.160,   4.160,
     4    4.213,    4.213,   4.213,   4.213,   4.213,   4.213,   4.213,
     5    4.329,    4.329,   4.329,   4.329,   4.329,   4.329,   4.329,
     6    4.488,    4.488,   4.488,   4.488,   4.488,   4.488,   4.488,
     7    4.696,    4.696,   4.696,   4.696,   4.696,   4.696,   4.696,
     8    4.957,    4.957,   4.957,   4.957,   4.957,   4.957,   4.957,
     9    5.276,    5.276,   5.276,   5.276,   5.276,   5.276,   5.276/
      data pf036/
     1    5.654,    5.654,   5.654,   5.654,   5.654,   5.654,   5.654,
     2    6.089,    6.089,   6.089,   6.089,   6.089,   6.089,   6.089,
     3    6.579,    6.579,   6.579,   6.579,   6.579,   6.579,   6.579,
     4    7.119,    7.119,   7.119,   7.119,   7.119,   7.119,   7.119,
     5    7.705,    7.705,   7.705,   7.705,   7.705,   7.705,   7.705,
     6    8.332,    8.332,   8.332,   8.332,   8.332,   8.332,   8.332,
     7    9.000,    9.000,   9.000,   9.000,   9.000,   9.000,   9.000,
     8    9.712,    9.712,   9.712,   9.712,   9.712,   9.712,   9.712,
     9   10.484,   10.484,  10.484,  10.484,  10.484,  10.484,  10.484/
      data pf037/
     1   11.340,   11.340,  11.340,  11.340,  11.340,  11.340,  11.340,
     2   12.326,   12.326,  12.326,  12.326,  12.326,  12.326,  12.326,
     3   13.518,   13.518,  13.518,  13.518,  13.518,  13.518,  13.518,
     4   15.028,   15.028,  15.028,  15.028,  15.028,  15.028,  15.027,
     5   17.035,   17.035,  17.035,  17.035,  17.034,  17.034,  17.033,
     6   19.821,   19.821,  19.821,  19.821,  19.821,  19.819,  19.815,
     7   23.845,   23.845,  23.844,  23.843,  23.842,  23.838,  23.822,
     8   29.849,   29.848,  29.846,  29.844,  29.838,  29.825,  29.774,
     9   39.023,   39.021,  39.016,  39.010,  38.992,  38.952,  38.804/
      data pf038/
     1   53.190,   53.185,  53.171,  53.156,  53.109,  53.004,  52.625,
     2   74.988,   74.975,  74.941,  74.906,  74.795,  74.550,  73.669,
     3  107.977,  107.948, 107.876, 107.799, 107.560, 107.035, 105.170,
     4    2.283,    2.283,   2.283,   2.283,   2.283,   2.283,   2.283,
     5    2.374,    2.374,   2.374,   2.374,   2.374,   2.374,   2.374,
     6    2.469,    2.469,   2.469,   2.469,   2.469,   2.469,   2.469,
     7    2.567,    2.567,   2.567,   2.567,   2.567,   2.567,   2.567,
     8    2.668,    2.668,   2.668,   2.668,   2.668,   2.668,   2.668,
     9    2.773,    2.773,   2.773,   2.773,   2.773,   2.773,   2.773/
      data pf039/
     1    2.880,    2.880,   2.880,   2.880,   2.880,   2.880,   2.880,
     2    2.990,    2.990,   2.990,   2.990,   2.990,   2.990,   2.990,
     3    3.102,    3.102,   3.102,   3.102,   3.102,   3.102,   3.102,
     4    3.217,    3.217,   3.217,   3.217,   3.217,   3.217,   3.217,
     5    3.334,    3.334,   3.334,   3.334,   3.334,   3.334,   3.334,
     6    3.452,    3.452,   3.452,   3.452,   3.452,   3.452,   3.452,
     7    3.573,    3.573,   3.573,   3.573,   3.573,   3.573,   3.573,
     8    3.694,    3.694,   3.694,   3.694,   3.694,   3.694,   3.694,
     9    3.817,    3.817,   3.817,   3.817,   3.817,   3.817,   3.817/
      data pf040/
     1    3.941,    3.941,   3.941,   3.941,   3.941,   3.941,   3.941,
     2    4.066,    4.066,   4.066,   4.066,   4.066,   4.066,   4.066,
     3    4.192,    4.192,   4.192,   4.192,   4.192,   4.192,   4.192,
     4    4.318,    4.318,   4.318,   4.318,   4.318,   4.318,   4.318,
     5    4.445,    4.445,   4.445,   4.445,   4.445,   4.445,   4.445,
     6    4.635,    4.635,   4.635,   4.635,   4.635,   4.635,   4.635,
     7    4.826,    4.826,   4.826,   4.826,   4.826,   4.826,   4.826,
     8    5.017,    5.017,   5.017,   5.017,   5.017,   5.017,   5.017,
     9    5.209,    5.209,   5.209,   5.209,   5.209,   5.209,   5.209/
      data pf041/
     1    5.401,    5.401,   5.401,   5.401,   5.401,   5.401,   5.401,
     2    5.593,    5.593,   5.593,   5.593,   5.593,   5.593,   5.593,
     3    5.787,    5.787,   5.787,   5.787,   5.787,   5.787,   5.787,
     4    5.981,    5.981,   5.981,   5.981,   5.981,   5.981,   5.981,
     5    6.177,    6.177,   6.177,   6.177,   6.177,   6.177,   6.177,
     6    6.374,    6.374,   6.374,   6.374,   6.374,   6.374,   6.374,
     7    6.707,    6.707,   6.707,   6.707,   6.707,   6.707,   6.707,
     8    7.046,    7.046,   7.046,   7.046,   7.046,   7.046,   7.046,
     9    7.391,    7.391,   7.391,   7.391,   7.391,   7.391,   7.391/
      data pf042/
     1    7.743,    7.743,   7.743,   7.743,   7.743,   7.743,   7.743,
     2    8.099,    8.099,   8.099,   8.099,   8.099,   8.099,   8.099,
     3    8.460,    8.460,   8.460,   8.460,   8.460,   8.460,   8.460,
     4    8.825,    8.825,   8.825,   8.825,   8.825,   8.825,   8.825,
     5    9.193,    9.193,   9.193,   9.193,   9.193,   9.193,   9.193,
     6    9.563,    9.563,   9.563,   9.563,   9.563,   9.563,   9.563,
     7    9.940,    9.940,   9.940,   9.940,   9.940,   9.940,   9.940,
     8   10.327,   10.327,  10.327,  10.327,  10.327,  10.327,  10.327,
     9   10.733,   10.733,  10.733,  10.733,  10.733,  10.733,  10.733/
      data pf043/
     1   11.171,   11.171,  11.171,  11.171,  11.171,  11.171,  11.171,
     2   11.662,   11.662,  11.662,  11.662,  11.662,  11.662,  11.662,
     3   12.233,   12.233,  12.233,  12.233,  12.233,  12.233,  12.233,
     4   12.922,   12.922,  12.922,  12.922,  12.922,  12.922,  12.922,
     5   13.783,   13.783,  13.783,  13.783,  13.783,  13.783,  13.783,
     6   14.889,   14.889,  14.889,  14.889,  14.889,  14.889,  14.889,
     7   16.345,   16.345,  16.345,  16.345,  16.345,  16.345,  16.345,
     8   18.301,   18.301,  18.301,  18.301,  18.301,  18.301,  18.301,
     9   20.976,   20.976,  20.976,  20.976,  20.976,  20.976,  20.976/
      data pf044/
     1   24.698,   24.697,  24.697,  24.697,  24.697,  24.696,  24.695,
     2   29.957,   29.956,  29.956,  29.956,  29.956,  29.953,  29.949,
     3   37.491,   37.488,  37.488,  37.488,  37.488,  37.477,  37.465,
     4   48.370,   48.362,  48.362,  48.362,  48.361,  48.333,  48.300,
     5   64.090,   64.068,  64.068,  64.068,  64.067,  63.999,  63.919,
     6    2.206,    2.206,   2.206,   2.206,   2.206,   2.206,   2.206,
     7    2.235,    2.235,   2.235,   2.235,   2.235,   2.235,   2.235,
     8    2.267,    2.267,   2.267,   2.267,   2.267,   2.267,   2.267,
     9    2.302,    2.302,   2.302,   2.302,   2.302,   2.302,   2.302/
      data pf045/
     1    2.339,    2.339,   2.339,   2.339,   2.339,   2.339,   2.339,
     2    2.379,    2.379,   2.379,   2.379,   2.379,   2.379,   2.379,
     3    2.421,    2.421,   2.421,   2.421,   2.421,   2.421,   2.421,
     4    2.466,    2.466,   2.466,   2.466,   2.466,   2.466,   2.466,
     5    2.514,    2.514,   2.514,   2.514,   2.514,   2.514,   2.514,
     6    2.563,    2.563,   2.563,   2.563,   2.563,   2.563,   2.563,
     7    2.615,    2.615,   2.615,   2.615,   2.615,   2.615,   2.615,
     8    2.669,    2.669,   2.669,   2.669,   2.669,   2.669,   2.669,
     9    2.725,    2.725,   2.725,   2.725,   2.725,   2.725,   2.725/
      data pf046/
     1    2.783,    2.783,   2.783,   2.783,   2.783,   2.783,   2.783,
     2    2.843,    2.843,   2.843,   2.843,   2.843,   2.843,   2.843,
     3    2.904,    2.904,   2.904,   2.904,   2.904,   2.904,   2.904,
     4    2.967,    2.967,   2.967,   2.967,   2.967,   2.967,   2.967,
     5    3.031,    3.031,   3.031,   3.031,   3.031,   3.031,   3.031,
     6    3.095,    3.095,   3.095,   3.095,   3.095,   3.095,   3.095,
     7    3.161,    3.161,   3.161,   3.161,   3.161,   3.161,   3.161,
     8    3.261,    3.261,   3.261,   3.261,   3.261,   3.261,   3.261,
     9    3.362,    3.362,   3.362,   3.362,   3.362,   3.362,   3.362/
      data pf047/
     1    3.464,    3.464,   3.464,   3.464,   3.464,   3.464,   3.464,
     2    3.565,    3.565,   3.565,   3.565,   3.565,   3.565,   3.565,
     3    3.666,    3.666,   3.666,   3.666,   3.666,   3.666,   3.666,
     4    3.767,    3.767,   3.767,   3.767,   3.767,   3.767,   3.767,
     5    3.866,    3.866,   3.866,   3.866,   3.866,   3.866,   3.866,
     6    3.963,    3.963,   3.963,   3.963,   3.963,   3.963,   3.963,
     7    4.059,    4.059,   4.059,   4.059,   4.059,   4.059,   4.059,
     8    4.152,    4.152,   4.152,   4.152,   4.152,   4.152,   4.152,
     9    4.302,    4.302,   4.302,   4.302,   4.302,   4.302,   4.302/
      data pf048/
     1    4.445,    4.445,   4.445,   4.445,   4.445,   4.445,   4.445,
     2    4.579,    4.579,   4.579,   4.579,   4.579,   4.579,   4.579,
     3    4.705,    4.705,   4.705,   4.705,   4.705,   4.705,   4.705,
     4    4.823,    4.823,   4.823,   4.823,   4.823,   4.823,   4.823,
     5    4.933,    4.933,   4.933,   4.933,   4.933,   4.933,   4.933,
     6    5.035,    5.035,   5.035,   5.035,   5.035,   5.035,   5.035,
     7    5.132,    5.132,   5.132,   5.132,   5.132,   5.132,   5.132,
     8    5.226,    5.226,   5.226,   5.226,   5.226,   5.226,   5.226,
     9    5.323,    5.323,   5.323,   5.323,   5.323,   5.323,   5.323/
      data pf049/
     1    5.427,    5.427,   5.427,   5.427,   5.427,   5.427,   5.427,
     2    5.549,    5.549,   5.549,   5.549,   5.549,   5.549,   5.549,
     3    5.701,    5.701,   5.701,   5.701,   5.701,   5.701,   5.701,
     4    5.898,    5.898,   5.898,   5.898,   5.898,   5.898,   5.898,
     5    6.157,    6.157,   6.157,   6.157,   6.157,   6.157,   6.157,
     6    6.498,    6.498,   6.498,   6.498,   6.498,   6.498,   6.498,
     7    6.949,    6.949,   6.949,   6.949,   6.949,   6.949,   6.949,
     8    7.540,    7.540,   7.540,   7.540,   7.540,   7.540,   7.540,
     9    8.316,    8.316,   8.316,   8.316,   8.316,   8.316,   8.316/
      data pf050/
     1    9.333,    9.333,   9.333,   9.333,   9.333,   9.333,   9.333,
     2   10.673,   10.673,  10.673,  10.673,  10.673,  10.673,  10.673,
     3   12.449,   12.449,  12.449,  12.449,  12.449,  12.449,  12.449,
     4   14.822,   14.822,  14.822,  14.822,  14.822,  14.822,  14.821,
     5   18.016,   18.016,  18.016,  18.015,  18.015,  18.014,  18.012,
     6   22.346,   22.346,  22.346,  22.344,  22.343,  22.341,  22.333,
     7   28.243,   28.243,  28.243,  28.240,  28.237,  28.230,  28.209,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf051/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf052/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf053/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf054/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     9    1.002,    1.002,   1.002,   1.002,   1.002,   1.002,   1.002/
      data pf055/
     1    1.005,    1.005,   1.005,   1.005,   1.005,   1.005,   1.005,
     2    1.012,    1.012,   1.012,   1.012,   1.012,   1.012,   1.012,
     3    1.024,    1.024,   1.024,   1.024,   1.024,   1.024,   1.024,
     4    1.047,    1.047,   1.047,   1.047,   1.047,   1.047,   1.047,
     5    1.084,    1.084,   1.084,   1.084,   1.084,   1.084,   1.084,
     6    1.142,    1.142,   1.142,   1.142,   1.142,   1.142,   1.142,
     7    1.228,    1.228,   1.228,   1.228,   1.228,   1.228,   1.228,
     8    1.350,    1.350,   1.350,   1.350,   1.350,   1.350,   1.350,
     9    1.517,    1.517,   1.517,   1.517,   1.517,   1.517,   1.517/
      data pf056/
     1    1.739,    1.739,   1.739,   1.739,   1.739,   1.739,   1.739,
     2    2.028,    2.028,   2.028,   2.028,   2.028,   2.028,   2.028,
     3    2.400,    2.400,   2.400,   2.400,   2.400,   2.400,   2.400,
     4    2.876,    2.876,   2.876,   2.876,   2.876,   2.876,   2.876,
     5    3.482,    3.482,   3.482,   3.482,   3.482,   3.482,   3.482,
     6    4.256,    4.256,   4.256,   4.256,   4.256,   4.256,   4.256,
     7    5.249,    5.249,   5.249,   5.249,   5.249,   5.249,   5.248,
     8    6.533,    6.533,   6.533,   6.533,   6.533,   6.532,   6.532,
     9    8.219,    8.219,   8.218,   8.218,   8.218,   8.216,   8.215/
      data pf057/
     1    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     2    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     3    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     4    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     5    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     6    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     7    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     8    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     9    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000/
      data pf058/
     1    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     2    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     3    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     4    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     5    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     6    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     7    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     8    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     9    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000/
      data pf059/
     1    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     2    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     3    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     4    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     5    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     6    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     7    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     8    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     9    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000/
      data pf060/
     1    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     2    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     3    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     4    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     5    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     6    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     7    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     8    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     9    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000/
      data pf061/
     1    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     2    2.000,    2.000,   2.000,   2.000,   2.000,   2.000,   2.000,
     3    2.001,    2.001,   2.001,   2.001,   2.001,   2.001,   2.001,
     4    2.002,    2.002,   2.002,   2.002,   2.002,   2.002,   2.002,
     5    2.004,    2.004,   2.004,   2.004,   2.004,   2.004,   2.004,
     6    2.010,    2.010,   2.010,   2.010,   2.010,   2.010,   2.010,
     7    2.020,    2.020,   2.020,   2.020,   2.020,   2.020,   2.020,
     8    2.037,    2.037,   2.037,   2.037,   2.037,   2.037,   2.037,
     9    2.064,    2.064,   2.064,   2.064,   2.064,   2.064,   2.064/
      data pf062/
     1    2.105,    2.105,   2.105,   2.105,   2.105,   2.105,   2.105,
     2    2.165,    2.165,   2.165,   2.165,   2.165,   2.165,   2.165,
     3    2.246,    2.246,   2.246,   2.246,   2.246,   2.246,   2.246,
     4    2.353,    2.353,   2.353,   2.353,   2.353,   2.353,   2.353,
     5    2.491,    2.491,   2.491,   2.491,   2.491,   2.491,   2.491,
     6    2.663,    2.663,   2.663,   2.663,   2.663,   2.663,   2.663,
     7    2.874,    2.874,   2.874,   2.874,   2.874,   2.874,   2.874,
     8    3.128,    3.128,   3.128,   3.128,   3.128,   3.128,   3.128,
     9    3.430,    3.430,   3.430,   3.430,   3.430,   3.430,   3.430/
      data pf063/
     1    3.787,    3.787,   3.787,   3.787,   3.787,   3.787,   3.787,
     2    4.211,    4.211,   4.211,   4.211,   4.211,   4.211,   4.211,
     3    9.354,    9.354,   9.354,   9.354,   9.354,   9.354,   9.354,
     4    9.387,    9.387,   9.387,   9.387,   9.387,   9.387,   9.387,
     5    9.421,    9.421,   9.421,   9.421,   9.421,   9.421,   9.421,
     6    9.457,    9.457,   9.457,   9.457,   9.457,   9.457,   9.457,
     7    9.495,    9.495,   9.495,   9.495,   9.495,   9.495,   9.495,
     8    9.537,    9.537,   9.537,   9.537,   9.537,   9.537,   9.537,
     9    9.584,    9.584,   9.584,   9.584,   9.584,   9.584,   9.584/
      data pf064/
     1    9.637,    9.637,   9.637,   9.637,   9.637,   9.637,   9.637,
     2    9.699,    9.699,   9.699,   9.699,   9.699,   9.699,   9.698,
     3    9.772,    9.772,   9.772,   9.772,   9.772,   9.772,   9.770,
     4    9.858,    9.858,   9.858,   9.858,   9.858,   9.858,   9.855,
     5    9.960,    9.960,   9.960,   9.960,   9.960,   9.960,   9.957,
     6   10.083,   10.082,  10.082,  10.082,  10.082,  10.082,  10.077,
     7   10.229,   10.229,  10.229,  10.229,  10.229,  10.229,  10.220,
     8   10.403,   10.403,  10.403,  10.403,  10.403,  10.403,  10.390,
     9   10.611,   10.611,  10.611,  10.611,  10.611,  10.610,  10.592/
      data pf065/
     1   10.858,   10.858,  10.858,  10.858,  10.857,  10.856,  10.829,
     2   11.150,   11.150,  11.150,  11.149,  11.149,  11.147,  11.108,
     3   11.494,   11.494,  11.493,  11.492,  11.491,  11.488,  11.433,
     4   11.898,   11.897,  11.896,  11.895,  11.892,  11.887,  11.809,
     5   12.634,   12.634,  12.629,  12.626,  12.621,  12.609,  12.483,
     6   13.561,   13.559,  13.548,  13.541,  13.529,  13.503,  13.303,
     7   14.722,   14.717,  14.693,  14.675,  14.649,  14.599,  14.289,
     8   16.177,   16.167,  16.112,  16.074,  16.019,  15.926,  15.457,
     9   18.008,   17.987,  17.870,  17.792,  17.683,  17.517,  16.822/
      data pf066/
     1   20.329,   20.285,  20.051,  19.897,  19.690,  19.404,  18.397,
     2   23.296,   23.212,  22.763,  22.472,  22.095,  21.620,  20.191,
     3   27.127,   26.973,  26.148,  25.621,  24.963,  24.200,  22.210,
     4   32.117,   31.843,  30.389,  29.471,  28.364,  27.175,  24.459,
     5   38.653,   38.183,  35.717,  34.176,  32.377,  30.578,  26.934,
     6   54.380,   53.319,  47.808,  44.423,  40.654,  37.275,  31.551,
     7   78.870,   76.678,  65.393,  58.565,  51.278,  45.346,  36.743,
     8  116.106,  111.918,  90.544,  77.783,  64.665,  54.864,  42.444,
     9  170.857,  163.400, 125.634, 103.350,  81.194,  65.852,  48.570/
      data pf067/
     1  248.339,  235.867, 173.143, 136.519, 101.164,  78.280,  55.027,
     2  353.721,  333.997, 235.411, 178.384, 124.757,  92.065,  61.714,
     3  491.573,  461.898, 314.381, 229.759, 152.010, 107.070,  68.531,
     4  665.346,  622.639, 411.369, 291.073, 182.806, 123.117,  75.382,
     5  876.975,  817.898, 526.913, 362.321, 216.879, 139.995,  82.182,
     6 1126.658, 1047.769, 660.704, 443.051, 253.832, 157.477,  88.852,
     7 1412.836, 1310.754, 811.612, 532.405, 293.168, 175.325,  95.330,
     8 1732.350, 1603.906, 977.792, 629.193, 334.323, 193.308, 101.564,
     9 2080.733, 1923.106,1156.840, 731.982, 376.702, 211.209, 107.513/
      data pf068/
     1 2452.574, 2263.394,1345.978, 839.197, 419.712, 228.834, 113.150,
     2 2841.920, 2619.331,1542.244, 949.224, 462.790, 246.011, 118.455,
     3 3242.643, 2985.336,1742.662,1060.485, 505.420, 262.602, 123.419,
     4 3648.772, 3355.984,1944.389,1171.511, 547.151, 278.493, 128.039,
     5 4054.742, 3726.227,2144.821,1280.989, 587.605, 293.601, 132.320,
     6 4455.578, 4091.563,2341.668,1387.789, 626.476, 307.869, 136.270,
     7 4847.004, 4448.129,2532.996,1490.979, 663.531, 321.261, 139.900,
     8 5225.494, 4792.746,2717.237,1589.826, 698.605, 333.763, 143.226,
     9 5588.269, 5122.915,2893.184,1683.784, 731.590, 345.377, 146.264/
      data pf069/
     1 5933.259, 5436.779,3059.966,1772.481, 762.435, 356.119, 149.031,
     2 6259.042, 5733.071,3217.014,1855.695, 791.131, 366.015, 151.546,
     3 6564.769, 6011.041,3364.020,1933.335, 817.704, 375.099, 153.827,
     4 6850.075, 6270.376,3500.899,2005.419, 842.212, 383.413, 155.891,
     5   15.556,   15.556,  15.556,  15.556,  15.556,  15.556,  15.556,
     6   15.791,   15.791,  15.791,  15.791,  15.791,  15.791,  15.791,
     7   16.041,   16.041,  16.041,  16.041,  16.041,  16.041,  16.041,
     8   16.305,   16.305,  16.305,  16.305,  16.305,  16.305,  16.305,
     9   16.584,   16.584,  16.584,  16.584,  16.584,  16.584,  16.584/
      data pf070/
     1   16.878,   16.878,  16.878,  16.878,  16.878,  16.878,  16.878,
     2   17.189,   17.189,  17.189,  17.189,  17.189,  17.189,  17.189,
     3   17.515,   17.515,  17.515,  17.515,  17.515,  17.515,  17.515,
     4   17.859,   17.859,  17.859,  17.859,  17.859,  17.859,  17.859,
     5   18.219,   18.219,  18.219,  18.219,  18.219,  18.219,  18.219,
     6   18.596,   18.596,  18.596,  18.596,  18.596,  18.596,  18.596,
     7   18.990,   18.990,  18.990,  18.990,  18.990,  18.990,  18.990,
     8   19.402,   19.402,  19.402,  19.402,  19.402,  19.402,  19.402,
     9   19.832,   19.832,  19.832,  19.832,  19.832,  19.832,  19.832/
      data pf071/
     1   20.280,   20.280,  20.280,  20.280,  20.280,  20.280,  20.280,
     2   20.746,   20.746,  20.746,  20.746,  20.746,  20.746,  20.746,
     3   21.230,   21.230,  21.230,  21.230,  21.230,  21.230,  21.230,
     4   21.732,   21.732,  21.732,  21.732,  21.732,  21.732,  21.732,
     5   22.254,   22.254,  22.254,  22.254,  22.254,  22.254,  22.254,
     6   22.794,   22.794,  22.794,  22.794,  22.794,  22.794,  22.794,
     7   23.641,   23.641,  23.641,  23.641,  23.641,  23.641,  23.641,
     8   24.534,   24.534,  24.534,  24.534,  24.534,  24.534,  24.534,
     9   25.473,   25.473,  25.473,  25.473,  25.473,  25.473,  25.473/
      data pf072/
     1   26.461,   26.461,  26.461,  26.461,  26.461,  26.461,  26.461,
     2   27.501,   27.501,  27.501,  27.501,  27.501,  27.501,  27.501,
     3   28.598,   28.598,  28.598,  28.598,  28.598,  28.598,  28.597,
     4   29.754,   29.754,  29.754,  29.754,  29.754,  29.754,  29.753,
     5   30.976,   30.976,  30.976,  30.976,  30.976,  30.976,  30.974,
     6   32.271,   32.271,  32.271,  32.271,  32.270,  32.269,  32.266,
     7   33.647,   33.647,  33.647,  33.647,  33.645,  33.642,  33.635,
     8   36.156,   36.156,  36.156,  36.156,  36.145,  36.132,  36.110,
     9   39.018,   39.018,  39.018,  39.018,  38.976,  38.931,  38.862/
      data pf073/
     1   42.411,   42.411,  42.411,  42.409,  42.271,  42.128,  41.943,
     2   46.669,   46.669,  46.669,  46.665,  46.260,  45.863,  45.412,
     3   52.393,   52.392,  52.392,  52.382,  51.324,  50.338,  49.341,
     4   60.565,   60.564,  60.564,  60.540,  58.053,  55.830,  53.805,
     5   72.662,   72.661,  72.661,  72.608,  67.280,  62.695,  58.880,
     6   90.709,   90.706,  90.706,  90.600,  80.095,  71.350,  64.635,
     7  117.238,  117.232, 117.232, 117.035,  97.795,  82.245,  71.122,
     8  155.145,  155.135, 155.135, 154.792, 121.798,  95.819,  78.368,
     9  207.440,  207.424, 207.424, 206.863, 153.505, 112.455,  86.372/
      data pf074/
     1  276.943,  276.918, 276.918, 276.047, 194.150, 132.429,  95.097,
     2  365.969,  365.932, 365.932, 364.643, 244.663, 155.882, 104.478,
     3  476.062,  476.010, 476.010, 474.181, 305.559, 182.799, 114.419,
     4  607.823,  607.751, 607.751, 605.255, 376.879, 213.005, 124.807,
     5  760.843,  760.746, 760.746, 757.453, 458.184, 246.185, 135.512,
     6  933.747,  933.622, 933.622, 929.404, 548.596, 281.905, 146.401,
     7 1124.328, 1124.172,1124.172,1118.914, 646.881, 319.652, 157.341,
     8 1329.741, 1329.550,1329.550,1323.151, 751.544, 358.862, 168.207,
     9 1546.723, 1546.495,1546.495,1538.872, 860.944, 398.963, 178.885/
      data pf075/
     1 1771.815, 1771.547,1771.547,1762.637, 973.389, 439.396, 189.277,
     2 2001.563, 2001.254,2001.254,1991.015,1087.234, 479.645, 199.302,
     3 2232.679, 2232.328,2232.328,2220.738,1200.942, 519.249, 208.895,
     4 2462.169, 2461.775,2461.775,2448.832,1313.141, 557.815, 218.007,
     5 2687.408, 2686.972,2686.972,2672.690,1422.651, 595.020, 226.606,
     6 2906.191, 2905.714,2905.714,2890.122,1528.502, 630.610, 234.672,
     7    9.237,    9.237,   9.237,   9.237,   9.237,   9.237,   9.237,
     8    9.269,    9.269,   9.269,   9.269,   9.269,   9.269,   9.269,
     9    9.300,    9.300,   9.300,   9.300,   9.300,   9.300,   9.300/
      data pf076/
     1    9.329,    9.329,   9.329,   9.329,   9.329,   9.329,   9.329,
     2    9.358,    9.358,   9.358,   9.358,   9.358,   9.358,   9.358,
     3    9.385,    9.385,   9.385,   9.385,   9.385,   9.385,   9.385,
     4    9.411,    9.411,   9.411,   9.411,   9.411,   9.411,   9.411,
     5    9.437,    9.437,   9.437,   9.437,   9.437,   9.437,   9.437,
     6    9.461,    9.461,   9.461,   9.461,   9.461,   9.461,   9.461,
     7    9.484,    9.484,   9.484,   9.484,   9.484,   9.484,   9.484,
     8    9.506,    9.506,   9.506,   9.506,   9.506,   9.506,   9.506,
     9    9.528,    9.528,   9.528,   9.528,   9.528,   9.528,   9.528/
      data pf077/
     1    9.548,    9.548,   9.548,   9.548,   9.548,   9.548,   9.548,
     2    9.568,    9.568,   9.568,   9.568,   9.568,   9.568,   9.568,
     3    9.587,    9.587,   9.587,   9.587,   9.587,   9.587,   9.587,
     4    9.605,    9.605,   9.605,   9.605,   9.605,   9.605,   9.605,
     5    9.622,    9.622,   9.622,   9.622,   9.622,   9.622,   9.622,
     6    9.639,    9.639,   9.639,   9.639,   9.639,   9.639,   9.639,
     7    9.655,    9.655,   9.655,   9.655,   9.655,   9.655,   9.655,
     8    9.670,    9.670,   9.670,   9.670,   9.670,   9.670,   9.670,
     9    9.693,    9.693,   9.693,   9.693,   9.693,   9.693,   9.693/
      data pf078/
     1    9.714,    9.714,   9.714,   9.714,   9.714,   9.714,   9.714,
     2    9.735,    9.735,   9.735,   9.735,   9.735,   9.735,   9.735,
     3    9.755,    9.755,   9.755,   9.755,   9.755,   9.755,   9.755,
     4    9.775,    9.775,   9.775,   9.775,   9.775,   9.775,   9.775,
     5    9.795,    9.795,   9.795,   9.795,   9.795,   9.795,   9.795,
     6    9.816,    9.816,   9.816,   9.816,   9.816,   9.816,   9.816,
     7    9.837,    9.837,   9.837,   9.837,   9.837,   9.837,   9.837,
     8    9.859,    9.859,   9.859,   9.859,   9.859,   9.859,   9.859,
     9    9.883,    9.883,   9.883,   9.883,   9.883,   9.883,   9.883/
      data pf079/
     1    9.927,    9.927,   9.927,   9.927,   9.927,   9.927,   9.927,
     2    9.979,    9.979,   9.979,   9.979,   9.979,   9.979,   9.979,
     3   10.039,   10.039,  10.039,  10.039,  10.039,  10.039,  10.039,
     4   10.112,   10.112,  10.112,  10.112,  10.112,  10.112,  10.111,
     5   10.199,   10.199,  10.199,  10.199,  10.199,  10.199,  10.198,
     6   10.305,   10.305,  10.305,  10.305,  10.305,  10.305,  10.304,
     7   10.439,   10.439,  10.439,  10.439,  10.439,  10.437,  10.435,
     8   10.615,   10.615,  10.615,  10.615,  10.615,  10.607,  10.599,
     9   10.857,   10.857,  10.857,  10.857,  10.857,  10.832,  10.810/
      data pf080/
     1   11.216,   11.216,  11.216,  11.216,  11.216,  11.144,  11.087,
     2   11.774,   11.774,  11.774,  11.774,  11.774,  11.592,  11.455,
     3   12.664,   12.664,  12.664,  12.664,  12.664,  12.248,  11.950,
     4   14.076,   14.076,  14.076,  14.076,  14.076,  13.210,  12.614,
     5   16.266,   16.266,  16.266,  16.266,  16.266,  14.599,  13.493,
     6   19.542,   19.542,  19.542,  19.542,  19.542,  16.553,  14.635,
     7   24.247,   24.247,  24.247,  24.247,  24.247,  19.219,  16.085,
     8   30.728,   30.728,  30.728,  30.728,  30.728,  22.733,  17.878,
     9   39.295,   39.295,  39.295,  39.295,  39.295,  27.209,  20.037/
      data pf081/
     1   50.193,   50.193,  50.193,  50.193,  50.193,  32.723,  22.568,
     2   63.569,   63.569,  63.569,  63.569,  63.569,  39.308,  25.462,
     3   79.455,   79.455,  79.455,  79.455,  79.455,  46.945,  28.692,
     4   97.769,   97.769,  97.769,  97.769,  97.769,  55.571,  32.220,
     5  118.322,  118.322, 118.322, 118.322, 118.322,  65.078,  35.994,
     6  140.832,  140.832, 140.832, 140.832, 140.832,  75.330,  39.958,
     7  164.953,  164.953, 164.953, 164.953, 164.953,  86.164,  44.052,
     8  190.298,  190.298, 190.298, 190.298, 190.298,  97.412,  48.216,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf082/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf083/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf084/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf085/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf086/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     4    1.002,    1.002,   1.002,   1.002,   1.002,   1.002,   1.002,
     5    1.006,    1.006,   1.006,   1.006,   1.006,   1.006,   1.006,
     6    1.015,    1.015,   1.015,   1.015,   1.015,   1.015,   1.015,
     7    1.039,    1.039,   1.039,   1.039,   1.039,   1.039,   1.038,
     8    1.089,    1.089,   1.089,   1.089,   1.089,   1.089,   1.089,
     9    1.195,    1.195,   1.195,   1.195,   1.195,   1.195,   1.191/
      data pf087/
     1    1.408,    1.408,   1.408,   1.408,   1.408,   1.408,   1.392,
     2    1.831,    1.831,   1.831,   1.831,   1.830,   1.829,   1.775,
     3    2.656,    2.656,   2.656,   2.656,   2.655,   2.649,   2.485,
     4    4.233,    4.233,   4.233,   4.232,   4.229,   4.214,   3.769,
     5    7.146,    7.146,   7.146,   7.143,   7.137,   7.097,   6.017,
     6   12.293,   12.293,  12.293,  12.286,  12.272,  12.184,   9.799,
     7   20.942,   20.942,  20.942,  20.927,  20.897,  20.716,  15.886,
     8   34.722,   34.722,  34.722,  34.693,  34.636,  34.293,  25.233,
     9   55.558,   55.558,  55.558,  55.507,  55.403,  54.796,  38.927/
      data pf088/
     1   85.515,   85.515,  85.515,  85.429,  85.256,  84.245,  58.092,
     2    4.102,    4.102,   4.102,   4.102,   4.102,   4.102,   4.102,
     3    4.116,    4.116,   4.116,   4.116,   4.116,   4.116,   4.116,
     4    4.132,    4.132,   4.132,   4.132,   4.132,   4.132,   4.132,
     5    4.149,    4.149,   4.149,   4.149,   4.149,   4.149,   4.149,
     6    4.168,    4.168,   4.168,   4.168,   4.168,   4.168,   4.168,
     7    4.188,    4.188,   4.188,   4.188,   4.188,   4.188,   4.188,
     8    4.209,    4.209,   4.209,   4.209,   4.209,   4.209,   4.209,
     9    4.231,    4.231,   4.231,   4.231,   4.231,   4.231,   4.231/
      data pf089/
     1    4.255,    4.255,   4.255,   4.255,   4.255,   4.255,   4.255,
     2    4.279,    4.279,   4.279,   4.279,   4.279,   4.279,   4.279,
     3    4.305,    4.305,   4.305,   4.305,   4.305,   4.305,   4.305,
     4    4.332,    4.332,   4.332,   4.332,   4.332,   4.332,   4.332,
     5    4.360,    4.360,   4.360,   4.360,   4.360,   4.360,   4.360,
     6    4.389,    4.389,   4.389,   4.389,   4.389,   4.389,   4.389,
     7    4.419,    4.419,   4.419,   4.419,   4.419,   4.419,   4.419,
     8    4.449,    4.449,   4.449,   4.449,   4.449,   4.449,   4.449,
     9    4.481,    4.481,   4.481,   4.481,   4.481,   4.481,   4.481/
      data pf090/
     1    4.513,    4.513,   4.513,   4.513,   4.513,   4.513,   4.513,
     2    4.545,    4.545,   4.545,   4.545,   4.545,   4.545,   4.545,
     3    4.578,    4.578,   4.578,   4.578,   4.578,   4.578,   4.578,
     4    4.628,    4.628,   4.628,   4.628,   4.628,   4.628,   4.628,
     5    4.678,    4.678,   4.678,   4.678,   4.678,   4.678,   4.678,
     6    4.729,    4.729,   4.729,   4.729,   4.729,   4.729,   4.729,
     7    4.780,    4.780,   4.780,   4.780,   4.780,   4.780,   4.780,
     8    4.830,    4.830,   4.830,   4.830,   4.830,   4.830,   4.830,
     9    4.880,    4.880,   4.880,   4.880,   4.880,   4.880,   4.880/
      data pf091/
     1    4.930,    4.930,   4.930,   4.930,   4.930,   4.930,   4.930,
     2    4.979,    4.979,   4.979,   4.979,   4.979,   4.979,   4.979,
     3    5.027,    5.027,   5.027,   5.027,   5.027,   5.027,   5.027,
     4    5.073,    5.073,   5.073,   5.073,   5.073,   5.073,   5.073,
     5    5.149,    5.149,   5.149,   5.149,   5.149,   5.149,   5.149,
     6    5.220,    5.220,   5.220,   5.220,   5.220,   5.220,   5.220,
     7    5.287,    5.287,   5.287,   5.287,   5.287,   5.287,   5.287,
     8    5.350,    5.350,   5.350,   5.350,   5.350,   5.350,   5.350,
     9    5.409,    5.409,   5.409,   5.409,   5.409,   5.409,   5.409/
      data pf092/
     1    5.464,    5.464,   5.464,   5.464,   5.464,   5.464,   5.464,
     2    5.515,    5.515,   5.515,   5.515,   5.515,   5.515,   5.515,
     3    5.561,    5.561,   5.561,   5.561,   5.561,   5.561,   5.561,
     4    5.604,    5.604,   5.604,   5.604,   5.604,   5.604,   5.604,
     5    5.645,    5.645,   5.645,   5.645,   5.645,   5.645,   5.645,
     6    5.684,    5.684,   5.684,   5.684,   5.684,   5.684,   5.684,
     7    5.726,    5.726,   5.726,   5.726,   5.726,   5.726,   5.726,
     8    5.779,    5.779,   5.779,   5.779,   5.779,   5.779,   5.779,
     9    5.858,    5.858,   5.858,   5.858,   5.858,   5.858,   5.858/
      data pf093/
     1    5.990,    5.990,   5.990,   5.990,   5.990,   5.990,   5.990,
     2    6.221,    6.221,   6.221,   6.221,   6.221,   6.221,   6.221,
     3    6.622,    6.622,   6.622,   6.622,   6.622,   6.622,   6.621,
     4    7.315,    7.315,   7.315,   7.315,   7.315,   7.314,   7.311,
     5    8.505,    8.505,   8.505,   8.505,   8.504,   8.502,   8.490,
     6   10.554,   10.554,  10.553,  10.551,  10.549,  10.542,  10.500,
     7   14.092,   14.092,  14.088,  14.082,  14.075,  14.055,  13.924,
     8   20.178,   20.177,  20.168,  20.151,  20.130,  20.074,  19.717,
     9   30.487,   30.485,  30.462,  30.420,  30.368,  30.230,  29.357/
      data pf094/
     1   47.479,   47.474,  47.423,  47.327,  47.210,  46.900,  44.965,
     2   74.482,   74.472,  74.366,  74.167,  73.927,  73.290,  69.356,
     3  115.643,  115.624, 115.421, 115.040, 114.581, 113.371, 105.967,
     4    5.346,    5.346,   5.346,   5.346,   5.346,   5.346,   5.346,
     5    5.386,    5.386,   5.386,   5.386,   5.386,   5.386,   5.386,
     6    5.428,    5.428,   5.428,   5.428,   5.428,   5.428,   5.428,
     7    5.472,    5.472,   5.472,   5.472,   5.472,   5.472,   5.472,
     8    5.519,    5.519,   5.519,   5.519,   5.519,   5.519,   5.519,
     9    5.568,    5.568,   5.568,   5.568,   5.568,   5.568,   5.568/
      data pf095/
     1    5.620,    5.620,   5.620,   5.620,   5.620,   5.620,   5.620,
     2    5.673,    5.673,   5.673,   5.673,   5.673,   5.673,   5.673,
     3    5.729,    5.729,   5.729,   5.729,   5.729,   5.729,   5.729,
     4    5.786,    5.786,   5.786,   5.786,   5.786,   5.786,   5.786,
     5    5.846,    5.846,   5.846,   5.846,   5.846,   5.846,   5.846,
     6    5.906,    5.906,   5.906,   5.906,   5.906,   5.906,   5.906,
     7    5.969,    5.969,   5.969,   5.969,   5.969,   5.969,   5.969,
     8    6.032,    6.032,   6.032,   6.032,   6.032,   6.032,   6.032,
     9    6.097,    6.097,   6.097,   6.097,   6.097,   6.097,   6.097/
      data pf096/
     1    6.163,    6.163,   6.163,   6.163,   6.163,   6.163,   6.163,
     2    6.230,    6.230,   6.230,   6.230,   6.230,   6.230,   6.230,
     3    6.298,    6.298,   6.298,   6.298,   6.298,   6.298,   6.298,
     4    6.367,    6.367,   6.367,   6.367,   6.367,   6.367,   6.367,
     5    6.437,    6.437,   6.437,   6.437,   6.437,   6.437,   6.437,
     6    6.543,    6.543,   6.543,   6.543,   6.543,   6.543,   6.543,
     7    6.651,    6.651,   6.651,   6.651,   6.651,   6.651,   6.651,
     8    6.762,    6.762,   6.762,   6.762,   6.762,   6.762,   6.762,
     9    6.874,    6.874,   6.874,   6.874,   6.874,   6.874,   6.874/
      data pf097/
     1    6.989,    6.989,   6.989,   6.989,   6.989,   6.989,   6.989,
     2    7.106,    7.106,   7.106,   7.106,   7.106,   7.106,   7.106,
     3    7.227,    7.227,   7.227,   7.227,   7.227,   7.227,   7.227,
     4    7.351,    7.351,   7.351,   7.351,   7.351,   7.351,   7.351,
     5    7.479,    7.479,   7.479,   7.479,   7.479,   7.479,   7.479,
     6    7.611,    7.611,   7.611,   7.611,   7.611,   7.611,   7.611,
     7    7.842,    7.842,   7.842,   7.842,   7.842,   7.842,   7.842,
     8    8.085,    8.085,   8.085,   8.085,   8.085,   8.085,   8.085,
     9    8.341,    8.341,   8.341,   8.341,   8.341,   8.341,   8.341/
      data pf098/
     1    8.610,    8.610,   8.610,   8.610,   8.610,   8.610,   8.610,
     2    8.890,    8.890,   8.890,   8.890,   8.890,   8.890,   8.890,
     3    9.180,    9.180,   9.180,   9.180,   9.180,   9.180,   9.180,
     4    9.477,    9.477,   9.477,   9.477,   9.477,   9.477,   9.477,
     5    9.780,    9.780,   9.780,   9.780,   9.780,   9.780,   9.780,
     6   10.085,   10.085,  10.085,  10.085,  10.085,  10.085,  10.085,
     7   10.392,   10.392,  10.392,  10.392,  10.392,  10.392,  10.392,
     8   10.700,   10.700,  10.700,  10.700,  10.700,  10.700,  10.700,
     9   11.012,   11.012,  11.012,  11.012,  11.012,  11.012,  11.012/
      data pf099/
     1   11.336,   11.336,  11.336,  11.336,  11.336,  11.336,  11.336,
     2   11.687,   11.687,  11.687,  11.687,  11.687,  11.687,  11.687,
     3   12.096,   12.096,  12.096,  12.096,  12.096,  12.096,  12.096,
     4   12.612,   12.612,  12.612,  12.612,  12.612,  12.612,  12.612,
     5   13.314,   13.314,  13.314,  13.314,  13.314,  13.314,  13.314,
     6   14.321,   14.321,  14.321,  14.321,  14.321,  14.321,  14.320,
     7   15.820,   15.820,  15.820,  15.820,  15.820,  15.820,  15.818,
     8   18.107,   18.107,  18.107,  18.107,  18.107,  18.105,  18.098,
     9   21.666,   21.666,  21.666,  21.666,  21.665,  21.656,  21.632/
      data pf100/
     1   27.301,   27.301,  27.300,  27.299,  27.296,  27.267,  27.184,
     2   36.327,   36.325,  36.323,  36.320,  36.312,  36.225,  35.980,
     3   50.809,   50.804,  50.799,  50.791,  50.769,  50.538,  49.898,
     4   73.811,   73.798,  73.785,  73.767,  73.715,  73.162,  71.656,
     5  109.561,  109.532, 109.504, 109.465, 109.352, 108.149, 104.921,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf101/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf102/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     5    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     6    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     7    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     8    4.003,    4.003,   4.003,   4.003,   4.003,   4.003,   4.003,
     9    4.006,    4.006,   4.006,   4.006,   4.006,   4.006,   4.006/
      data pf103/
     1    4.009,    4.009,   4.009,   4.009,   4.009,   4.009,   4.009,
     2    4.015,    4.015,   4.015,   4.015,   4.015,   4.015,   4.015,
     3    4.023,    4.023,   4.023,   4.023,   4.023,   4.023,   4.023,
     4    4.035,    4.035,   4.035,   4.035,   4.035,   4.035,   4.035,
     5    4.051,    4.051,   4.051,   4.051,   4.051,   4.051,   4.051,
     6    4.072,    4.072,   4.072,   4.072,   4.072,   4.072,   4.072,
     7    4.101,    4.101,   4.101,   4.101,   4.101,   4.101,   4.101,
     8    4.139,    4.139,   4.139,   4.139,   4.139,   4.139,   4.139,
     9    4.224,    4.224,   4.224,   4.224,   4.224,   4.224,   4.224/
      data pf104/
     1    4.346,    4.346,   4.346,   4.346,   4.346,   4.346,   4.346,
     2    4.510,    4.510,   4.510,   4.510,   4.510,   4.510,   4.510,
     3    4.723,    4.723,   4.723,   4.723,   4.723,   4.723,   4.723,
     4    4.990,    4.990,   4.990,   4.990,   4.990,   4.990,   4.990,
     5    5.316,    5.316,   5.316,   5.316,   5.316,   5.316,   5.316,
     6    5.699,    5.699,   5.699,   5.699,   5.699,   5.699,   5.699,
     7    6.140,    6.140,   6.140,   6.140,   6.140,   6.140,   6.140,
     8    6.635,    6.635,   6.635,   6.635,   6.635,   6.635,   6.635,
     9    7.179,    7.179,   7.179,   7.179,   7.179,   7.179,   7.179/
      data pf105/
     1    7.768,    7.768,   7.768,   7.768,   7.768,   7.768,   7.768,
     2    8.398,    8.398,   8.398,   8.398,   8.398,   8.398,   8.398,
     3    9.067,    9.067,   9.067,   9.067,   9.067,   9.067,   9.067,
     4    9.779,    9.779,   9.779,   9.779,   9.779,   9.779,   9.779,
     5   10.549,   10.549,  10.549,  10.549,  10.549,  10.549,  10.549,
     6   11.401,   11.401,  11.401,  11.401,  11.401,  11.401,  11.401,
     7   12.382,   12.382,  12.382,  12.382,  12.382,  12.382,  12.382,
     8   13.562,   13.562,  13.562,  13.562,  13.562,  13.562,  13.562,
     9   15.049,   15.049,  15.049,  15.049,  15.049,  15.049,  15.049/
      data pf106/
     1   17.007,   17.007,  17.007,  17.007,  17.007,  17.007,  17.007,
     2   19.682,   19.682,  19.682,  19.682,  19.682,  19.682,  19.682,
     3   23.456,   23.456,  23.456,  23.455,  23.455,  23.454,  23.453,
     4   28.929,   28.929,  28.929,  28.928,  28.927,  28.923,  28.919,
     5   37.054,   37.054,  37.054,  37.051,  37.047,  37.035,  37.022,
     6   49.314,   49.314,  49.313,  49.306,  49.294,  49.259,  49.219,
     7   67.932,   67.930,  67.929,  67.910,  67.878,  67.784,  67.681,
     8    1.740,    1.740,   1.740,   1.740,   1.740,   1.740,   1.740,
     9    1.807,    1.807,   1.807,   1.807,   1.807,   1.807,   1.807/
      data pf107/
     1    1.877,    1.877,   1.877,   1.877,   1.877,   1.877,   1.877,
     2    1.952,    1.952,   1.952,   1.952,   1.952,   1.952,   1.952,
     3    2.030,    2.030,   2.030,   2.030,   2.030,   2.030,   2.030,
     4    2.112,    2.112,   2.112,   2.112,   2.112,   2.112,   2.112,
     5    2.197,    2.197,   2.197,   2.197,   2.197,   2.197,   2.197,
     6    2.286,    2.286,   2.286,   2.286,   2.286,   2.286,   2.286,
     7    2.379,    2.379,   2.379,   2.379,   2.379,   2.379,   2.379,
     8    2.475,    2.475,   2.475,   2.475,   2.475,   2.475,   2.475,
     9    2.575,    2.575,   2.575,   2.575,   2.575,   2.575,   2.575/
      data pf108/
     1    2.677,    2.677,   2.677,   2.677,   2.677,   2.677,   2.677,
     2    2.783,    2.783,   2.783,   2.783,   2.783,   2.783,   2.783,
     3    2.892,    2.892,   2.892,   2.892,   2.892,   2.892,   2.892,
     4    3.004,    3.004,   3.004,   3.004,   3.004,   3.004,   3.004,
     5    3.118,    3.118,   3.118,   3.118,   3.118,   3.118,   3.118,
     6    3.234,    3.234,   3.234,   3.234,   3.234,   3.234,   3.234,
     7    3.352,    3.352,   3.352,   3.352,   3.352,   3.352,   3.352,
     8    3.473,    3.473,   3.473,   3.473,   3.473,   3.473,   3.473,
     9    3.595,    3.595,   3.595,   3.595,   3.595,   3.595,   3.595/
      data pf109/
     1    3.782,    3.782,   3.782,   3.782,   3.782,   3.782,   3.782,
     2    3.971,    3.971,   3.971,   3.971,   3.971,   3.971,   3.971,
     3    4.163,    4.163,   4.163,   4.163,   4.163,   4.163,   4.163,
     4    4.358,    4.358,   4.358,   4.358,   4.358,   4.358,   4.358,
     5    4.554,    4.554,   4.554,   4.554,   4.554,   4.554,   4.554,
     6    4.752,    4.752,   4.752,   4.752,   4.752,   4.752,   4.752,
     7    4.952,    4.952,   4.952,   4.952,   4.952,   4.952,   4.952,
     8    5.154,    5.154,   5.154,   5.154,   5.154,   5.154,   5.154,
     9    5.358,    5.358,   5.358,   5.358,   5.358,   5.358,   5.358/
      data pf110/
     1    5.564,    5.564,   5.564,   5.564,   5.564,   5.564,   5.564,
     2    5.912,    5.912,   5.912,   5.912,   5.912,   5.912,   5.912,
     3    6.267,    6.267,   6.267,   6.267,   6.267,   6.267,   6.267,
     4    6.628,    6.628,   6.628,   6.628,   6.628,   6.628,   6.628,
     5    6.996,    6.996,   6.996,   6.996,   6.996,   6.996,   6.996,
     6    7.370,    7.370,   7.370,   7.370,   7.370,   7.370,   7.370,
     7    7.750,    7.750,   7.750,   7.750,   7.750,   7.750,   7.750,
     8    8.134,    8.134,   8.134,   8.134,   8.134,   8.134,   8.134,
     9    8.521,    8.521,   8.521,   8.521,   8.521,   8.521,   8.521/
      data pf111/
     1    8.910,    8.910,   8.910,   8.910,   8.910,   8.910,   8.910,
     2    9.301,    9.301,   9.301,   9.301,   9.301,   9.301,   9.301,
     3    9.697,    9.697,   9.697,   9.697,   9.697,   9.697,   9.697,
     4   10.102,   10.102,  10.102,  10.102,  10.102,  10.102,  10.102,
     5   10.526,   10.526,  10.526,  10.526,  10.526,  10.526,  10.526,
     6   10.980,   10.980,  10.980,  10.980,  10.980,  10.980,  10.980,
     7   11.485,   11.485,  11.485,  11.485,  11.485,  11.485,  11.485,
     8   12.070,   12.070,  12.070,  12.070,  12.070,  12.070,  12.070,
     9   12.771,   12.771,  12.771,  12.771,  12.771,  12.771,  12.771/
      data pf112/
     1   13.642,   13.642,  13.642,  13.642,  13.642,  13.642,  13.642,
     2   14.755,   14.755,  14.755,  14.755,  14.755,  14.755,  14.755,
     3   16.211,   16.211,  16.211,  16.211,  16.211,  16.211,  16.211,
     4   18.151,   18.151,  18.151,  18.151,  18.151,  18.151,  18.151,
     5   20.779,   20.779,  20.779,  20.779,  20.779,  20.779,  20.778,
     6   24.385,   24.385,  24.385,  24.385,  24.385,  24.385,  24.384,
     7   29.401,   29.401,  29.401,  29.401,  29.400,  29.399,  29.397,
     8   36.459,   36.459,  36.459,  36.458,  36.456,  36.453,  36.444,
     9   46.484,   46.484,  46.482,  46.479,  46.474,  46.466,  46.438/
      data pf113/
     1    2.076,    2.076,   2.076,   2.076,   2.076,   2.076,   2.076,
     2    2.090,    2.090,   2.090,   2.090,   2.090,   2.090,   2.090,
     3    2.107,    2.107,   2.107,   2.107,   2.107,   2.107,   2.107,
     4    2.126,    2.126,   2.126,   2.126,   2.126,   2.126,   2.126,
     5    2.148,    2.148,   2.148,   2.148,   2.148,   2.148,   2.148,
     6    2.171,    2.171,   2.171,   2.171,   2.171,   2.171,   2.171,
     7    2.197,    2.197,   2.197,   2.197,   2.197,   2.197,   2.197,
     8    2.226,    2.226,   2.226,   2.226,   2.226,   2.226,   2.226,
     9    2.257,    2.257,   2.257,   2.257,   2.257,   2.257,   2.257/
      data pf114/
     1    2.291,    2.291,   2.291,   2.291,   2.291,   2.291,   2.291,
     2    2.327,    2.327,   2.327,   2.327,   2.327,   2.327,   2.327,
     3    2.366,    2.366,   2.366,   2.366,   2.366,   2.366,   2.366,
     4    2.408,    2.408,   2.408,   2.408,   2.408,   2.408,   2.408,
     5    2.452,    2.452,   2.452,   2.452,   2.452,   2.452,   2.452,
     6    2.499,    2.499,   2.499,   2.499,   2.499,   2.499,   2.499,
     7    2.548,    2.548,   2.548,   2.548,   2.548,   2.548,   2.548,
     8    2.599,    2.599,   2.599,   2.599,   2.599,   2.599,   2.599,
     9    2.652,    2.652,   2.652,   2.652,   2.652,   2.652,   2.652/
      data pf115/
     1    2.708,    2.708,   2.708,   2.708,   2.708,   2.708,   2.708,
     2    2.765,    2.765,   2.765,   2.765,   2.765,   2.765,   2.765,
     3    2.855,    2.855,   2.855,   2.855,   2.855,   2.855,   2.855,
     4    2.947,    2.947,   2.947,   2.947,   2.947,   2.947,   2.947,
     5    3.043,    3.043,   3.043,   3.043,   3.043,   3.043,   3.043,
     6    3.141,    3.141,   3.141,   3.141,   3.141,   3.141,   3.141,
     7    3.240,    3.240,   3.240,   3.240,   3.240,   3.240,   3.240,
     8    3.341,    3.341,   3.341,   3.341,   3.341,   3.341,   3.341,
     9    3.443,    3.443,   3.443,   3.443,   3.443,   3.443,   3.443/
      data pf116/
     1    3.544,    3.544,   3.544,   3.544,   3.544,   3.544,   3.544,
     2    3.646,    3.646,   3.646,   3.646,   3.646,   3.646,   3.646,
     3    3.746,    3.746,   3.746,   3.746,   3.746,   3.746,   3.746,
     4    3.911,    3.911,   3.911,   3.911,   3.911,   3.911,   3.911,
     5    4.071,    4.071,   4.071,   4.071,   4.071,   4.071,   4.071,
     6    4.224,    4.224,   4.224,   4.224,   4.224,   4.224,   4.224,
     7    4.371,    4.371,   4.371,   4.371,   4.371,   4.371,   4.371,
     8    4.510,    4.510,   4.510,   4.510,   4.510,   4.510,   4.510,
     9    4.641,    4.641,   4.641,   4.641,   4.641,   4.641,   4.641/
      data pf117/
     1    4.763,    4.763,   4.763,   4.763,   4.763,   4.763,   4.763,
     2    4.879,    4.879,   4.879,   4.879,   4.879,   4.879,   4.879,
     3    4.988,    4.988,   4.988,   4.988,   4.988,   4.988,   4.988,
     4    5.094,    5.094,   5.094,   5.094,   5.094,   5.094,   5.094,
     5    5.202,    5.202,   5.202,   5.202,   5.202,   5.202,   5.202,
     6    5.319,    5.319,   5.319,   5.319,   5.319,   5.319,   5.319,
     7    5.452,    5.452,   5.452,   5.452,   5.452,   5.452,   5.452,
     8    5.616,    5.616,   5.616,   5.616,   5.616,   5.616,   5.616,
     9    5.824,    5.824,   5.824,   5.824,   5.824,   5.824,   5.824/
      data pf118/
     1    6.096,    6.096,   6.096,   6.096,   6.096,   6.096,   6.096,
     2    6.452,    6.452,   6.452,   6.452,   6.452,   6.452,   6.452,
     3    6.921,    6.921,   6.921,   6.921,   6.921,   6.921,   6.921,
     4    7.538,    7.538,   7.538,   7.538,   7.538,   7.538,   7.538,
     5    8.347,    8.347,   8.347,   8.347,   8.347,   8.347,   8.347,
     6    9.412,    9.412,   9.412,   9.412,   9.412,   9.412,   9.412,
     7   10.816,   10.816,  10.816,  10.816,  10.816,  10.816,  10.816,
     8   12.672,   12.672,  12.672,  12.672,  12.672,  12.672,  12.672,
     9   15.136,   15.136,  15.136,  15.136,  15.136,  15.136,  15.136/
      data pf119/
     1   18.418,   18.418,  18.418,  18.418,  18.418,  18.417,  18.417,
     2   22.807,   22.807,  22.807,  22.806,  22.806,  22.804,  22.803,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf120/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf121/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf122/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf123/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     5    1.002,    1.002,   1.002,   1.002,   1.002,   1.002,   1.002,
     6    1.006,    1.006,   1.006,   1.006,   1.006,   1.006,   1.006,
     7    1.013,    1.013,   1.013,   1.013,   1.013,   1.013,   1.013,
     8    1.026,    1.026,   1.026,   1.026,   1.026,   1.026,   1.026,
     9    1.050,    1.050,   1.050,   1.050,   1.050,   1.050,   1.050/
      data pf124/
     1    1.089,    1.089,   1.089,   1.089,   1.089,   1.089,   1.089,
     2    1.150,    1.150,   1.150,   1.150,   1.150,   1.150,   1.150,
     3    1.239,    1.239,   1.239,   1.239,   1.239,   1.239,   1.239,
     4    1.366,    1.366,   1.366,   1.366,   1.366,   1.366,   1.366,
     5    1.538,    1.538,   1.538,   1.538,   1.538,   1.538,   1.538,
     6    1.767,    1.767,   1.767,   1.767,   1.767,   1.767,   1.767,
     7    2.064,    2.064,   2.064,   2.064,   2.064,   2.064,   2.064,
     8    2.447,    2.447,   2.447,   2.447,   2.447,   2.447,   2.447,
     9    2.936,    2.936,   2.936,   2.936,   2.936,   2.936,   2.936/
      data pf125/
     1    3.559,    3.559,   3.559,   3.559,   3.559,   3.559,   3.559,
     2    4.352,    4.352,   4.352,   4.352,   4.352,   4.352,   4.352,
     3    5.362,    5.362,   5.362,   5.362,   5.362,   5.362,   5.362,
     4    6.658,    6.658,   6.658,   6.658,   6.658,   6.658,   6.657,
     5   18.535,   18.535,  18.535,  18.535,  18.535,  18.535,  18.535,
     6   18.752,   18.752,  18.752,  18.752,  18.752,  18.752,  18.752,
     7   18.985,   18.985,  18.985,  18.985,  18.985,  18.985,  18.985,
     8   19.235,   19.235,  19.235,  19.235,  19.235,  19.235,  19.235,
     9   19.507,   19.507,  19.507,  19.507,  19.507,  19.507,  19.507/
      data pf126/
     1   19.804,   19.804,  19.804,  19.804,  19.804,  19.804,  19.803,
     2   20.128,   20.128,  20.128,  20.128,  20.128,  20.128,  20.128,
     3   20.484,   20.484,  20.484,  20.484,  20.484,  20.484,  20.484,
     4   20.877,   20.877,  20.877,  20.877,  20.877,  20.877,  20.876,
     5   21.312,   21.312,  21.312,  21.312,  21.312,  21.312,  21.310,
     6   21.793,   21.793,  21.793,  21.793,  21.793,  21.793,  21.790,
     7   22.327,   22.327,  22.327,  22.327,  22.327,  22.327,  22.321,
     8   22.920,   22.920,  22.920,  22.920,  22.920,  22.920,  22.911,
     9   23.581,   23.581,  23.581,  23.581,  23.581,  23.581,  23.566/
      data pf127/
     1   24.318,   24.317,  24.317,  24.317,  24.317,  24.317,  24.293,
     2   25.139,   25.139,  25.139,  25.139,  25.139,  25.138,  25.101,
     3   26.056,   26.056,  26.056,  26.055,  26.055,  26.054,  25.997,
     4   27.081,   27.081,  27.080,  27.080,  27.079,  27.077,  26.991,
     5   28.227,   28.227,  28.226,  28.225,  28.224,  28.220,  28.093,
     6   29.511,   29.510,  29.508,  29.506,  29.503,  29.496,  29.312,
     7   31.733,   31.730,  31.724,  31.719,  31.713,  31.697,  31.383,
     8   34.379,   34.372,  34.356,  34.342,  34.328,  34.294,  33.775,
     9   37.545,   37.529,  37.488,  37.457,  37.425,  37.356,  36.524/
      data pf128/
     1   41.362,   41.325,  41.232,  41.160,  41.092,  40.958,  39.662,
     2   46.010,   45.929,  45.726,  45.572,  45.434,  45.185,  43.220,
     3   51.744,   51.574,  51.154,  50.842,  50.574,  50.131,  47.224,
     4   58.920,   58.584,  57.756,  57.154,  56.655,  55.895,  51.695,
     5   68.042,   67.404,  65.844,  64.730,  63.839,  62.583,  56.648,
     6   79.799,   78.639,  75.824,  73.844,  72.314,  70.303,  62.092,
     7   95.116,   93.090,  88.205,  84.822,  82.286,  79.164,  68.025,
     8  131.993,  127.261, 115.968, 108.320, 102.854,  96.755,  78.982,
     9  189.917,  179.842, 156.008, 140.187, 129.345, 118.258,  91.202/
      data pf129/
     1  279.274,  259.512, 213.134, 182.893, 162.928, 144.026, 104.554,
     2  412.955,  376.930, 292.988, 239.112, 204.704, 174.276, 118.868,
     3  605.594,  544.075, 401.634, 311.493, 255.591, 209.058, 133.941,
     4  872.307,  773.192, 544.990, 402.378, 316.216, 248.241, 149.555,
     5 1227.144, 1075.526, 728.194, 513.542, 386.834, 291.515, 165.489,
     6 1681.515, 1460.060, 955.010, 645.975, 467.280, 338.413, 181.529,
     7 2242.879, 1932.476,1227.386, 799.750, 556.974, 388.337, 197.474,
     8 2913.895, 2494.504,1545.219, 973.996, 654.956, 440.605, 213.148,
     9 3692.133, 3143.729,1906.344,1166.966, 759.962, 494.488, 228.399/
      data pf130/
     1 4570.325, 3873.835,2306.737,1376.177, 870.513, 549.252, 243.103,
     2 5537.061, 4675.195,2740.862,1598.609, 985.009, 604.189, 257.162,
     3 6577.779, 5535.696,3202.113,1830.909,1101.824, 658.649, 270.507,
     4 7675.888, 6441.655,3683.277,2069.608,1219.385, 712.055, 283.088,
     5 8813.880, 7378.722,4176.982,2311.297,1336.234, 763.916, 294.879,
     6 9974.329, 8332.684,4676.071,2552.781,1451.076, 813.833, 305.870,
     711140.706, 9290.118,5173.903,2791.186,1562.802, 861.495, 316.067,
     812297.992,10238.870,5664.560,3024.025,1670.504, 906.677, 325.486,
     913433.066,11168.362,6142.975,3249.236,1773.475, 949.232, 334.153/
      data pf131/
     114534.923,12069.755,6604.986,3465.182,1871.195, 989.078, 342.101,
     215594.715,12935.977,7047.331,3670.636,1963.316,1026.195, 349.368,
     316605.689,13761.657,7467.596,3864.749,2049.642,1060.606, 355.993,
     417563.033,14543.000,7864.145,4047.004,2130.106,1092.375, 362.018,
     518463.662,15277.609,8236.023,4217.170,2204.748,1121.595, 367.486,
     619305.992,15964.296,8582.854,4375.258,2273.691,1148.380, 372.439,
     7   38.022,   38.022,  38.022,  38.022,  38.022,  38.022,  38.022,
     8   38.750,   38.750,  38.750,  38.750,  38.750,  38.750,  38.750,
     9   39.484,   39.484,  39.484,  39.484,  39.484,  39.484,  39.484/
      data pf132/
     1   40.226,   40.226,  40.226,  40.226,  40.226,  40.226,  40.226,
     2   40.979,   40.979,  40.979,  40.979,  40.979,  40.979,  40.979,
     3   41.744,   41.744,  41.744,  41.744,  41.744,  41.744,  41.744,
     4   42.525,   42.525,  42.525,  42.525,  42.525,  42.525,  42.525,
     5   43.324,   43.324,  43.324,  43.324,  43.324,  43.324,  43.324,
     6   44.144,   44.144,  44.144,  44.144,  44.144,  44.144,  44.144,
     7   44.987,   44.987,  44.987,  44.987,  44.987,  44.987,  44.987,
     8   45.858,   45.858,  45.858,  45.858,  45.858,  45.858,  45.858,
     9   46.759,   46.759,  46.759,  46.759,  46.759,  46.759,  46.759/
      data pf133/
     1   47.694,   47.694,  47.694,  47.694,  47.694,  47.694,  47.694,
     2   48.666,   48.666,  48.666,  48.666,  48.666,  48.666,  48.666,
     3   49.680,   49.680,  49.680,  49.680,  49.680,  49.680,  49.680,
     4   50.737,   50.737,  50.737,  50.737,  50.737,  50.737,  50.737,
     5   51.842,   51.842,  51.842,  51.842,  51.842,  51.842,  51.842,
     6   52.999,   52.999,  52.999,  52.999,  52.999,  52.999,  52.999,
     7   54.210,   54.210,  54.210,  54.210,  54.210,  54.210,  54.210,
     8   55.479,   55.479,  55.479,  55.479,  55.479,  55.479,  55.479,
     9   57.497,   57.497,  57.497,  57.497,  57.497,  57.497,  57.497/
      data pf134/
     1   59.663,   59.663,  59.663,  59.663,  59.663,  59.663,  59.663,
     2   61.987,   61.987,  61.987,  61.987,  61.987,  61.987,  61.987,
     3   64.480,   64.480,  64.480,  64.480,  64.480,  64.480,  64.480,
     4   67.153,   67.153,  67.153,  67.153,  67.153,  67.153,  67.153,
     5   70.018,   70.018,  70.018,  70.018,  70.018,  70.018,  70.018,
     6   73.090,   73.090,  73.090,  73.090,  73.090,  73.089,  73.089,
     7   76.384,   76.384,  76.384,  76.384,  76.384,  76.384,  76.382,
     8   79.922,   79.922,  79.922,  79.922,  79.921,  79.920,  79.917,
     9   83.727,   83.727,  83.727,  83.727,  83.725,  83.722,  83.714/
      data pf135/
     1   90.755,   90.754,  90.753,  90.751,  90.743,  90.729,  90.700,
     2   98.839,   98.834,  98.829,  98.823,  98.790,  98.735,  98.637,
     3  108.368,  108.349, 108.330, 108.305, 108.188, 108.001, 107.715,
     4  120.040,  119.979, 119.915, 119.832, 119.465, 118.908, 118.165,
     5  135.100,  134.923, 134.737, 134.498, 133.480, 132.004, 130.261,
     6  155.641,  155.178, 154.698, 154.087, 151.558, 148.041, 144.306,
     7  184.928,  183.842, 182.720, 181.313, 175.620, 167.995, 160.616,
     8  227.649,  225.327, 222.938, 219.975, 208.243, 193.040, 179.489,
     9  289.974,  285.401, 280.716, 274.963, 252.616, 224.490, 201.173/
      data pf136/
     1  379.341,  370.978, 362.438, 352.045, 312.358, 263.685, 225.833,
     2  503.946,  489.622, 475.040, 457.435, 391.218, 311.854, 253.527,
     3  671.989,  648.851, 625.359, 597.197, 492.698, 369.980, 284.190,
     4  890.806,  855.330, 819.397, 776.590, 619.657, 438.676, 317.631,
     5 1166.042, 1114.119,1061.638, 999.469, 773.984, 518.105, 353.545,
     6 1500.997, 1428.085,1354.530,1267.829, 956.367, 607.950, 391.531,
     7 1896.253, 1797.579,1698.203,1581.585,1166.211, 707.437, 431.118,
     8 2349.607, 2220.389,2090.450,1938.569,1401.687, 815.395, 471.797,
     9 2856.289, 2691.965,2526.945,2334.740,1659.898, 930.348, 513.050/
      data pf137/
     1 3409.402, 3205.824,3001.630,2764.543,1937.127,1050.626, 554.371,
     2 4000.496, 3754.096,3507.214,3221.363,2229.114,1174.468, 595.294,
     3 4620.192, 4328.088,4035.693,3697.988,2531.345,1300.124, 635.399,
     4 5258.777, 4918.842,4578.856,4187.058,2839.308,1425.932, 674.331,
     5 5906.731, 5517.603,5128.711,4681.440,3148.710,1550.382, 711.798,
     6 6555.144, 6116.202,5677.823,5174.525,3455.635,1672.155, 747.570,
     7 7196.013, 6707.324,6219.556,5660.435,3756.653,1790.145, 781.484,
     8 7822.429, 7284.672,6748.215,6134.143,4048.877,1903.465, 813.428,
     9   17.922,   17.922,  17.922,  17.922,  17.922,  17.922,  17.922/
      data pf138/
     1   18.052,   18.052,  18.052,  18.052,  18.052,  18.052,  18.052,
     2   18.181,   18.181,  18.181,  18.181,  18.181,  18.181,  18.181,
     3   18.307,   18.307,  18.307,  18.307,  18.307,  18.307,  18.307,
     4   18.432,   18.432,  18.432,  18.432,  18.432,  18.432,  18.432,
     5   18.556,   18.556,  18.556,  18.556,  18.556,  18.556,  18.556,
     6   18.680,   18.680,  18.680,  18.680,  18.680,  18.680,  18.680,
     7   18.804,   18.804,  18.804,  18.804,  18.804,  18.804,  18.804,
     8   18.930,   18.930,  18.930,  18.930,  18.930,  18.930,  18.930,
     9   19.057,   19.057,  19.057,  19.057,  19.057,  19.057,  19.057/
      data pf139/
     1   19.188,   19.188,  19.188,  19.188,  19.188,  19.188,  19.188,
     2   19.322,   19.322,  19.322,  19.322,  19.322,  19.322,  19.322,
     3   19.460,   19.460,  19.460,  19.460,  19.460,  19.460,  19.460,
     4   19.604,   19.604,  19.604,  19.604,  19.604,  19.604,  19.604,
     5   19.753,   19.753,  19.753,  19.753,  19.753,  19.753,  19.753,
     6   19.910,   19.910,  19.910,  19.910,  19.910,  19.910,  19.910,
     7   20.074,   20.074,  20.074,  20.074,  20.074,  20.074,  20.074,
     8   20.247,   20.247,  20.247,  20.247,  20.247,  20.247,  20.247,
     9   20.429,   20.429,  20.429,  20.429,  20.429,  20.429,  20.429/
      data pf140/
     1   20.620,   20.620,  20.620,  20.620,  20.620,  20.620,  20.620,
     2   20.928,   20.928,  20.928,  20.928,  20.928,  20.928,  20.928,
     3   21.260,   21.260,  21.260,  21.260,  21.260,  21.260,  21.260,
     4   21.619,   21.619,  21.619,  21.619,  21.619,  21.619,  21.619,
     5   22.006,   22.006,  22.006,  22.006,  22.006,  22.006,  22.006,
     6   22.420,   22.420,  22.420,  22.420,  22.420,  22.420,  22.420,
     7   22.864,   22.864,  22.864,  22.864,  22.864,  22.864,  22.864,
     8   23.336,   23.336,  23.336,  23.336,  23.336,  23.336,  23.336,
     9   23.837,   23.837,  23.837,  23.837,  23.837,  23.837,  23.837/
      data pf141/
     1   24.366,   24.366,  24.366,  24.366,  24.366,  24.366,  24.366,
     2   24.924,   24.924,  24.924,  24.924,  24.924,  24.924,  24.924,
     3   25.917,   25.917,  25.917,  25.917,  25.917,  25.917,  25.917,
     4   26.991,   26.991,  26.991,  26.991,  26.991,  26.991,  26.991,
     5   28.149,   28.149,  28.149,  28.149,  28.149,  28.149,  28.149,
     6   29.397,   29.397,  29.397,  29.397,  29.397,  29.397,  29.397,
     7   30.750,   30.750,  30.750,  30.750,  30.750,  30.750,  30.749,
     8   32.227,   32.227,  32.227,  32.227,  32.227,  32.226,  32.225,
     9   33.865,   33.865,  33.865,  33.865,  33.865,  33.861,  33.856/
      data pf142/
     1   35.729,   35.729,  35.729,  35.729,  35.729,  35.712,  35.692,
     2   37.946,   37.946,  37.946,  37.946,  37.946,  37.882,  37.812,
     3   40.753,   40.753,  40.753,  40.753,  40.752,  40.550,  40.340,
     4   44.589,   44.589,  44.589,  44.589,  44.588,  44.021,  43.461,
     5   50.202,   50.202,  50.202,  50.202,  50.200,  48.779,  47.435,
     6   58.771,   58.771,  58.771,  58.771,  58.766,  55.544,  52.611,
     7   71.993,   71.993,  71.993,  71.993,  71.981,  65.299,  59.417,
     8   92.103,   92.103,  92.103,  92.103,  92.079,  79.276,  68.337,
     9  121.793,  121.793, 121.793, 121.793, 121.751,  98.893,  79.877/
      data pf143/
     1  164.030,  164.030, 164.030, 164.030, 163.957, 125.641,  94.508,
     2  221.774,  221.774, 221.774, 221.774, 221.656, 160.935, 112.621,
     3  297.668,  297.668, 297.668, 297.668, 297.487, 205.960, 134.477,
     4  393.741,  393.741, 393.741, 393.741, 393.476, 261.533, 160.177,
     5  511.174,  511.174, 511.174, 511.174, 510.802, 328.012, 189.649,
     6  650.169,  650.169, 650.169, 650.169, 649.666, 405.252, 222.650,
     7  809.931,  809.931, 809.931, 809.931, 809.272, 492.621, 258.791,
     8  988.744,  988.744, 988.744, 988.744, 987.907, 589.059, 297.568,
     9 1184.138, 1184.138,1184.138,1184.138,1183.100, 693.169, 338.400/
      data pf144/
     1 1393.089, 1393.089,1393.089,1393.089,1391.833, 803.329, 380.665,
     2    8.612,    8.612,   8.612,   8.612,   8.612,   8.612,   8.612,
     3    8.667,    8.667,   8.667,   8.667,   8.667,   8.667,   8.667,
     4    8.720,    8.720,   8.720,   8.720,   8.720,   8.720,   8.720,
     5    8.771,    8.771,   8.771,   8.771,   8.771,   8.771,   8.771,
     6    8.821,    8.821,   8.821,   8.821,   8.821,   8.821,   8.821,
     7    8.868,    8.868,   8.868,   8.868,   8.868,   8.868,   8.868,
     8    8.914,    8.914,   8.914,   8.914,   8.914,   8.914,   8.914,
     9    8.959,    8.959,   8.959,   8.959,   8.959,   8.959,   8.959/
      data pf145/
     1    9.001,    9.001,   9.001,   9.001,   9.001,   9.001,   9.001,
     2    9.043,    9.043,   9.043,   9.043,   9.043,   9.043,   9.043,
     3    9.082,    9.082,   9.082,   9.082,   9.082,   9.082,   9.082,
     4    9.120,    9.120,   9.120,   9.120,   9.120,   9.120,   9.120,
     5    9.157,    9.157,   9.157,   9.157,   9.157,   9.157,   9.157,
     6    9.192,    9.192,   9.192,   9.192,   9.192,   9.192,   9.192,
     7    9.226,    9.226,   9.226,   9.226,   9.226,   9.226,   9.226,
     8    9.259,    9.259,   9.259,   9.259,   9.259,   9.259,   9.259,
     9    9.290,    9.290,   9.290,   9.290,   9.290,   9.290,   9.290/
      data pf146/
     1    9.320,    9.320,   9.320,   9.320,   9.320,   9.320,   9.320,
     2    9.349,    9.349,   9.349,   9.349,   9.349,   9.349,   9.349,
     3    9.377,    9.377,   9.377,   9.377,   9.377,   9.377,   9.377,
     4    9.416,    9.416,   9.416,   9.416,   9.416,   9.416,   9.416,
     5    9.453,    9.453,   9.453,   9.453,   9.453,   9.453,   9.453,
     6    9.488,    9.488,   9.488,   9.488,   9.488,   9.488,   9.488,
     7    9.521,    9.521,   9.521,   9.521,   9.521,   9.521,   9.521,
     8    9.552,    9.552,   9.552,   9.552,   9.552,   9.552,   9.552,
     9    9.581,    9.581,   9.581,   9.581,   9.581,   9.581,   9.581/
      data pf147/
     1    9.608,    9.608,   9.608,   9.608,   9.608,   9.608,   9.608,
     2    9.633,    9.633,   9.633,   9.633,   9.633,   9.633,   9.633,
     3    9.657,    9.657,   9.657,   9.657,   9.657,   9.657,   9.657,
     4    9.679,    9.679,   9.679,   9.679,   9.679,   9.679,   9.679,
     5    9.713,    9.713,   9.713,   9.713,   9.713,   9.713,   9.713,
     6    9.744,    9.744,   9.744,   9.744,   9.744,   9.744,   9.744,
     7    9.772,    9.772,   9.772,   9.772,   9.772,   9.772,   9.772,
     8    9.797,    9.797,   9.797,   9.797,   9.797,   9.797,   9.797,
     9    9.821,    9.821,   9.821,   9.821,   9.821,   9.821,   9.821/
      data pf148/
     1    9.844,    9.844,   9.844,   9.844,   9.844,   9.844,   9.844,
     2    9.867,    9.867,   9.867,   9.867,   9.867,   9.867,   9.867,
     3    9.894,    9.894,   9.894,   9.894,   9.894,   9.894,   9.894,
     4    9.926,    9.926,   9.926,   9.926,   9.926,   9.926,   9.926,
     5    9.969,    9.969,   9.969,   9.969,   9.969,   9.969,   9.968,
     6   10.027,   10.027,  10.027,  10.027,  10.027,  10.027,  10.026,
     7   10.115,   10.115,  10.115,  10.115,  10.115,  10.115,  10.111,
     8   10.255,   10.255,  10.255,  10.255,  10.255,  10.254,  10.243,
     9   10.492,   10.492,  10.492,  10.492,  10.492,  10.492,  10.457/
      data pf149/
     1   10.910,   10.910,  10.910,  10.910,  10.910,  10.910,  10.814,
     2   11.646,   11.646,  11.646,  11.646,  11.646,  11.645,  11.409,
     3   12.913,   12.913,  12.913,  12.913,  12.913,  12.911,  12.383,
     4   15.011,   15.011,  15.011,  15.011,  15.011,  15.007,  13.925,
     5   18.325,   18.325,  18.325,  18.325,  18.325,  18.316,  16.267,
     6   23.306,   23.306,  23.306,  23.306,  23.306,  23.290,  19.670,
     7   30.440,   30.440,  30.440,  30.440,  30.440,  30.413,  24.398,
     8   40.198,   40.198,  40.198,  40.198,  40.198,  40.155,  30.699,
     9   52.986,   52.986,  52.986,  52.986,  52.986,  52.919,  38.767/
      data pf150/
     1   69.098,   69.098,  69.098,  69.098,  69.098,  69.001,  48.728,
     2   88.684,   88.684,  88.684,  88.684,  88.684,  88.550,  60.620,
     3  111.735,  111.735, 111.735, 111.735, 111.735, 111.555,  74.396,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf151/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf152/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf153/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf154/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001/
      data pf155/
     1    1.004,    1.004,   1.004,   1.004,   1.004,   1.004,   1.004,
     2    1.012,    1.012,   1.012,   1.012,   1.012,   1.012,   1.012,
     3    1.031,    1.031,   1.031,   1.031,   1.031,   1.031,   1.031,
     4    1.072,    1.072,   1.072,   1.072,   1.072,   1.072,   1.072,
     5    1.153,    1.153,   1.153,   1.153,   1.153,   1.153,   1.153,
     6    1.308,    1.308,   1.308,   1.308,   1.308,   1.308,   1.308,
     7    1.594,    1.594,   1.594,   1.594,   1.594,   1.593,   1.592,
     8    2.115,    2.115,   2.115,   2.114,   2.113,   2.112,   2.107,
     9    3.061,    3.061,   3.060,   3.059,   3.055,   3.050,   3.034/
      data pf156/
     1    4.768,    4.767,   4.766,   4.762,   4.751,   4.734,   4.686,
     2    7.800,    7.797,   7.794,   7.783,   7.753,   7.708,   7.583,
     3   13.030,   13.023,  13.015,  12.989,  12.918,  12.810,  12.514,
     4   21.713,   21.699,  21.682,  21.624,  21.468,  21.233,  20.597,
     5   35.504,   35.476,  35.442,  35.325,  35.010,  34.541,  33.282,
     6    4.036,    4.036,   4.036,   4.036,   4.036,   4.036,   4.036,
     7    4.043,    4.043,   4.043,   4.043,   4.043,   4.043,   4.043,
     8    4.051,    4.051,   4.051,   4.051,   4.051,   4.051,   4.051,
     9    4.061,    4.061,   4.061,   4.061,   4.061,   4.061,   4.061/
      data pf157/
     1    4.071,    4.071,   4.071,   4.071,   4.071,   4.071,   4.071,
     2    4.082,    4.082,   4.082,   4.082,   4.082,   4.082,   4.082,
     3    4.095,    4.095,   4.095,   4.095,   4.095,   4.095,   4.095,
     4    4.109,    4.109,   4.109,   4.109,   4.109,   4.109,   4.109,
     5    4.124,    4.124,   4.124,   4.124,   4.124,   4.124,   4.124,
     6    4.141,    4.141,   4.141,   4.141,   4.141,   4.141,   4.141,
     7    4.159,    4.159,   4.159,   4.159,   4.159,   4.159,   4.159,
     8    4.178,    4.178,   4.178,   4.178,   4.178,   4.178,   4.178,
     9    4.199,    4.199,   4.199,   4.199,   4.199,   4.199,   4.199/
      data pf158/
     1    4.220,    4.220,   4.220,   4.220,   4.220,   4.220,   4.220,
     2    4.243,    4.243,   4.243,   4.243,   4.243,   4.243,   4.243,
     3    4.267,    4.267,   4.267,   4.267,   4.267,   4.267,   4.267,
     4    4.293,    4.293,   4.293,   4.293,   4.293,   4.293,   4.293,
     5    4.319,    4.319,   4.319,   4.319,   4.319,   4.319,   4.319,
     6    4.347,    4.347,   4.347,   4.347,   4.347,   4.347,   4.347,
     7    4.375,    4.375,   4.375,   4.375,   4.375,   4.375,   4.375,
     8    4.420,    4.420,   4.420,   4.420,   4.420,   4.420,   4.420,
     9    4.466,    4.466,   4.466,   4.466,   4.466,   4.466,   4.466/
      data pf159/
     1    4.513,    4.513,   4.513,   4.513,   4.513,   4.513,   4.513,
     2    4.562,    4.562,   4.562,   4.562,   4.562,   4.562,   4.562,
     3    4.612,    4.612,   4.612,   4.612,   4.612,   4.612,   4.612,
     4    4.662,    4.662,   4.662,   4.662,   4.662,   4.662,   4.662,
     5    4.713,    4.713,   4.713,   4.713,   4.713,   4.713,   4.713,
     6    4.764,    4.764,   4.764,   4.764,   4.764,   4.764,   4.764,
     7    4.814,    4.814,   4.814,   4.814,   4.814,   4.814,   4.814,
     8    4.865,    4.865,   4.865,   4.865,   4.865,   4.865,   4.865,
     9    4.947,    4.947,   4.947,   4.947,   4.947,   4.947,   4.947/
      data pf160/
     1    5.027,    5.027,   5.027,   5.027,   5.027,   5.027,   5.027,
     2    5.105,    5.105,   5.105,   5.105,   5.105,   5.105,   5.105,
     3    5.178,    5.178,   5.178,   5.178,   5.178,   5.178,   5.178,
     4    5.248,    5.248,   5.248,   5.248,   5.248,   5.248,   5.248,
     5    5.314,    5.314,   5.314,   5.314,   5.314,   5.314,   5.314,
     6    5.375,    5.375,   5.375,   5.375,   5.375,   5.375,   5.375,
     7    5.432,    5.432,   5.432,   5.432,   5.432,   5.432,   5.432,
     8    5.485,    5.485,   5.485,   5.485,   5.485,   5.485,   5.485,
     9    5.535,    5.535,   5.535,   5.535,   5.535,   5.535,   5.535/
      data pf161/
     1    5.581,    5.581,   5.581,   5.581,   5.581,   5.581,   5.581,
     2    5.625,    5.625,   5.625,   5.625,   5.625,   5.625,   5.625,
     3    5.672,    5.672,   5.672,   5.672,   5.672,   5.672,   5.672,
     4    5.727,    5.727,   5.727,   5.727,   5.727,   5.727,   5.727,
     5    5.806,    5.806,   5.806,   5.806,   5.806,   5.806,   5.806,
     6    5.934,    5.934,   5.934,   5.934,   5.934,   5.934,   5.934,
     7    6.149,    6.149,   6.149,   6.149,   6.149,   6.149,   6.149,
     8    6.516,    6.516,   6.516,   6.516,   6.516,   6.516,   6.516,
     9    7.131,    7.131,   7.131,   7.131,   7.131,   7.131,   7.131/
      data pf162/
     1    8.151,    8.151,   8.151,   8.151,   8.151,   8.151,   8.150,
     2    9.831,    9.831,   9.831,   9.831,   9.831,   9.830,   9.826,
     3   12.606,   12.606,  12.606,  12.605,  12.604,  12.601,  12.588,
     4   17.212,   17.212,  17.212,  17.210,  17.207,  17.196,  17.155,
     5   24.862,   24.862,  24.862,  24.858,  24.848,  24.818,  24.700,
     6   37.447,   37.447,  37.445,  37.437,  37.410,  37.334,  37.036,
     7   57.715,   57.715,  57.711,  57.691,  57.629,  57.454,  56.773,
     8    5.150,    5.150,   5.150,   5.150,   5.150,   5.150,   5.150,
     9    5.173,    5.173,   5.173,   5.173,   5.173,   5.173,   5.173/
      data pf163/
     1    5.199,    5.199,   5.199,   5.199,   5.199,   5.199,   5.199,
     2    5.227,    5.227,   5.227,   5.227,   5.227,   5.227,   5.227,
     3    5.258,    5.258,   5.258,   5.258,   5.258,   5.258,   5.258,
     4    5.291,    5.291,   5.291,   5.291,   5.291,   5.291,   5.291,
     5    5.327,    5.327,   5.327,   5.327,   5.327,   5.327,   5.327,
     6    5.365,    5.365,   5.365,   5.365,   5.365,   5.365,   5.365,
     7    5.406,    5.406,   5.406,   5.406,   5.406,   5.406,   5.406,
     8    5.450,    5.450,   5.450,   5.450,   5.450,   5.450,   5.450,
     9    5.496,    5.496,   5.496,   5.496,   5.496,   5.496,   5.496/
      data pf164/
     1    5.544,    5.544,   5.544,   5.544,   5.544,   5.544,   5.544,
     2    5.595,    5.595,   5.595,   5.595,   5.595,   5.595,   5.595,
     3    5.648,    5.648,   5.648,   5.648,   5.648,   5.648,   5.648,
     4    5.703,    5.703,   5.703,   5.703,   5.703,   5.703,   5.703,
     5    5.760,    5.760,   5.760,   5.760,   5.760,   5.760,   5.760,
     6    5.818,    5.818,   5.818,   5.818,   5.818,   5.818,   5.818,
     7    5.879,    5.879,   5.879,   5.879,   5.879,   5.879,   5.879,
     8    5.942,    5.942,   5.942,   5.942,   5.942,   5.942,   5.942,
     9    6.006,    6.006,   6.006,   6.006,   6.006,   6.006,   6.006/
      data pf165/
     1    6.105,    6.105,   6.105,   6.105,   6.105,   6.105,   6.105,
     2    6.207,    6.207,   6.207,   6.207,   6.207,   6.207,   6.207,
     3    6.313,    6.313,   6.313,   6.313,   6.313,   6.313,   6.313,
     4    6.421,    6.421,   6.421,   6.421,   6.421,   6.421,   6.421,
     5    6.533,    6.533,   6.533,   6.533,   6.533,   6.533,   6.533,
     6    6.648,    6.648,   6.648,   6.648,   6.648,   6.648,   6.648,
     7    6.767,    6.767,   6.767,   6.767,   6.767,   6.767,   6.767,
     8    6.890,    6.890,   6.890,   6.890,   6.890,   6.890,   6.890,
     9    7.016,    7.016,   7.016,   7.016,   7.016,   7.016,   7.016/
      data pf166/
     1    7.147,    7.147,   7.147,   7.147,   7.147,   7.147,   7.147,
     2    7.375,    7.375,   7.375,   7.375,   7.375,   7.375,   7.375,
     3    7.616,    7.616,   7.616,   7.616,   7.616,   7.616,   7.616,
     4    7.871,    7.871,   7.871,   7.871,   7.871,   7.871,   7.871,
     5    8.140,    8.140,   8.140,   8.140,   8.140,   8.140,   8.140,
     6    8.421,    8.421,   8.421,   8.421,   8.421,   8.421,   8.421,
     7    8.714,    8.714,   8.714,   8.714,   8.714,   8.714,   8.714,
     8    9.017,    9.017,   9.017,   9.017,   9.017,   9.017,   9.017,
     9    9.326,    9.326,   9.326,   9.326,   9.326,   9.326,   9.326/
      data pf167/
     1    9.641,    9.641,   9.641,   9.641,   9.641,   9.641,   9.641,
     2    9.958,    9.958,   9.958,   9.958,   9.958,   9.958,   9.958,
     3   10.275,   10.275,  10.275,  10.275,  10.275,  10.275,  10.275,
     4   10.594,   10.594,  10.594,  10.594,  10.594,  10.594,  10.594,
     5   10.915,   10.915,  10.915,  10.915,  10.915,  10.915,  10.915,
     6   11.247,   11.247,  11.247,  11.247,  11.247,  11.247,  11.247,
     7   11.605,   11.605,  11.605,  11.605,  11.605,  11.605,  11.605,
     8   12.019,   12.019,  12.019,  12.019,  12.019,  12.019,  12.019,
     9   12.537,   12.537,  12.537,  12.537,  12.537,  12.537,  12.537/
      data pf168/
     1   13.234,   13.234,  13.234,  13.234,  13.234,  13.234,  13.234,
     2   14.225,   14.225,  14.225,  14.225,  14.225,  14.225,  14.225,
     3   15.678,   15.678,  15.678,  15.678,  15.678,  15.678,  15.678,
     4   17.850,   17.850,  17.850,  17.850,  17.850,  17.850,  17.849,
     5   21.135,   21.135,  21.135,  21.135,  21.135,  21.134,  21.132,
     6   26.162,   26.162,  26.162,  26.162,  26.160,  26.159,  26.149,
     7   33.937,   33.937,  33.937,  33.935,  33.931,  33.926,  33.891,
     8   46.051,   46.051,  46.051,  46.045,  46.033,  46.019,  45.913,
     9   64.933,   64.933,  64.931,  64.915,  64.884,  64.845,  64.562/
      data pf169/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf170/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf171/
     1    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     2    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     3    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     4    4.003,    4.003,   4.003,   4.003,   4.003,   4.003,   4.003,
     5    4.005,    4.005,   4.005,   4.005,   4.005,   4.005,   4.005,
     6    4.008,    4.008,   4.008,   4.008,   4.008,   4.008,   4.008,
     7    4.013,    4.013,   4.013,   4.013,   4.013,   4.013,   4.013,
     8    4.020,    4.020,   4.020,   4.020,   4.020,   4.020,   4.020,
     9    4.030,    4.030,   4.030,   4.030,   4.030,   4.030,   4.030/
      data pf172/
     1    4.045,    4.045,   4.045,   4.045,   4.045,   4.045,   4.045,
     2    4.065,    4.065,   4.065,   4.065,   4.065,   4.065,   4.065,
     3    4.091,    4.091,   4.091,   4.091,   4.091,   4.091,   4.091,
     4    4.154,    4.154,   4.154,   4.154,   4.154,   4.154,   4.154,
     5    4.246,    4.246,   4.246,   4.246,   4.246,   4.246,   4.246,
     6    4.375,    4.375,   4.375,   4.375,   4.375,   4.375,   4.375,
     7    4.549,    4.549,   4.549,   4.549,   4.549,   4.549,   4.549,
     8    4.772,    4.772,   4.772,   4.772,   4.772,   4.772,   4.772,
     9    5.051,    5.051,   5.051,   5.051,   5.051,   5.051,   5.051/
      data pf173/
     1    5.387,    5.387,   5.387,   5.387,   5.387,   5.387,   5.387,
     2    5.781,    5.781,   5.781,   5.781,   5.781,   5.781,   5.781,
     3    6.232,    6.232,   6.232,   6.232,   6.232,   6.232,   6.232,
     4    6.736,    6.736,   6.736,   6.736,   6.736,   6.736,   6.736,
     5    7.288,    7.288,   7.288,   7.288,   7.288,   7.288,   7.288,
     6    7.884,    7.884,   7.884,   7.884,   7.884,   7.884,   7.884,
     7    8.519,    8.519,   8.519,   8.519,   8.519,   8.519,   8.519,
     8    9.193,    9.193,   9.193,   9.193,   9.193,   9.193,   9.193,
     9    9.911,    9.911,   9.911,   9.911,   9.911,   9.911,   9.911/
      data pf174/
     1   10.688,   10.688,  10.688,  10.688,  10.688,  10.688,  10.688,
     2   11.553,   11.553,  11.553,  11.553,  11.553,  11.553,  11.553,
     3   12.553,   12.553,  12.553,  12.553,  12.553,  12.553,  12.553,
     4   13.763,   13.763,  13.763,  13.763,  13.763,  13.763,  13.763,
     5   15.292,   15.292,  15.292,  15.292,  15.292,  15.292,  15.292,
     6   17.301,   17.301,  17.301,  17.301,  17.301,  17.301,  17.301,
     7   20.027,   20.027,  20.027,  20.027,  20.027,  20.027,  20.027,
     8   23.820,   23.820,  23.820,  23.820,  23.820,  23.820,  23.820,
     9   29.218,   29.218,  29.218,  29.218,  29.218,  29.217,  29.215/
      data pf175/
     1   37.046,   37.046,  37.046,  37.046,  37.045,  37.043,  37.035,
     2   48.576,   48.576,  48.576,  48.575,  48.573,  48.566,  48.542,
     3    1.176,    1.176,   1.176,   1.176,   1.176,   1.176,   1.176,
     4    1.202,    1.202,   1.202,   1.202,   1.202,   1.202,   1.202,
     5    1.231,    1.231,   1.231,   1.231,   1.231,   1.231,   1.231,
     6    1.262,    1.262,   1.262,   1.262,   1.262,   1.262,   1.262,
     7    1.296,    1.296,   1.296,   1.296,   1.296,   1.296,   1.296,
     8    1.334,    1.334,   1.334,   1.334,   1.334,   1.334,   1.334,
     9    1.374,    1.374,   1.374,   1.374,   1.374,   1.374,   1.374/
      data pf176/
     1    1.418,    1.418,   1.418,   1.418,   1.418,   1.418,   1.418,
     2    1.466,    1.466,   1.466,   1.466,   1.466,   1.466,   1.466,
     3    1.517,    1.517,   1.517,   1.517,   1.517,   1.517,   1.517,
     4    1.571,    1.571,   1.571,   1.571,   1.571,   1.571,   1.571,
     5    1.630,    1.630,   1.630,   1.630,   1.630,   1.630,   1.630,
     6    1.692,    1.692,   1.692,   1.692,   1.692,   1.692,   1.692,
     7    1.758,    1.758,   1.758,   1.758,   1.758,   1.758,   1.758,
     8    1.828,    1.828,   1.828,   1.828,   1.828,   1.828,   1.828,
     9    1.903,    1.903,   1.903,   1.903,   1.903,   1.903,   1.903/
      data pf177/
     1    1.981,    1.981,   1.981,   1.981,   1.981,   1.981,   1.981,
     2    2.063,    2.063,   2.063,   2.063,   2.063,   2.063,   2.063,
     3    2.149,    2.149,   2.149,   2.149,   2.149,   2.149,   2.149,
     4    2.239,    2.239,   2.239,   2.239,   2.239,   2.239,   2.239,
     5    2.380,    2.380,   2.380,   2.380,   2.380,   2.380,   2.380,
     6    2.531,    2.531,   2.531,   2.531,   2.531,   2.531,   2.531,
     7    2.689,    2.689,   2.689,   2.689,   2.689,   2.689,   2.689,
     8    2.854,    2.854,   2.854,   2.854,   2.854,   2.854,   2.854,
     9    3.027,    3.027,   3.027,   3.027,   3.027,   3.027,   3.027/
      data pf178/
     1    3.206,    3.206,   3.206,   3.206,   3.206,   3.206,   3.206,
     2    3.391,    3.391,   3.391,   3.391,   3.391,   3.391,   3.391,
     3    3.582,    3.582,   3.582,   3.582,   3.582,   3.582,   3.582,
     4    3.778,    3.778,   3.778,   3.778,   3.778,   3.778,   3.778,
     5    3.980,    3.980,   3.980,   3.980,   3.980,   3.980,   3.980,
     6    4.327,    4.327,   4.327,   4.327,   4.327,   4.327,   4.327,
     7    4.686,    4.686,   4.686,   4.686,   4.686,   4.686,   4.686,
     8    5.058,    5.058,   5.058,   5.058,   5.058,   5.058,   5.058,
     9    5.442,    5.442,   5.442,   5.442,   5.442,   5.442,   5.442/
      data pf179/
     1    5.837,    5.837,   5.837,   5.837,   5.837,   5.837,   5.837,
     2    6.241,    6.241,   6.241,   6.241,   6.241,   6.241,   6.241,
     3    6.655,    6.655,   6.655,   6.655,   6.655,   6.655,   6.655,
     4    7.075,    7.075,   7.075,   7.075,   7.075,   7.075,   7.075,
     5    7.501,    7.501,   7.501,   7.501,   7.501,   7.501,   7.501,
     6    7.932,    7.932,   7.932,   7.932,   7.932,   7.932,   7.932,
     7    8.368,    8.368,   8.368,   8.368,   8.368,   8.368,   8.368,
     8    8.811,    8.811,   8.811,   8.811,   8.811,   8.811,   8.811,
     9    9.265,    9.265,   9.265,   9.265,   9.265,   9.265,   9.265/
      data pf180/
     1    9.739,    9.739,   9.739,   9.739,   9.739,   9.739,   9.739,
     2   10.245,   10.245,  10.245,  10.245,  10.245,  10.245,  10.245,
     3   10.805,   10.805,  10.805,  10.805,  10.805,  10.805,  10.805,
     4   11.447,   11.447,  11.447,  11.447,  11.447,  11.447,  11.447,
     5   12.209,   12.209,  12.209,  12.209,  12.209,  12.209,  12.209,
     6   13.145,   13.145,  13.145,  13.145,  13.145,  13.145,  13.145,
     7   14.331,   14.331,  14.331,  14.331,  14.331,  14.331,  14.331,
     8   15.869,   15.869,  15.869,  15.869,  15.869,  15.869,  15.869,
     9   17.903,   17.903,  17.903,  17.903,  17.903,  17.903,  17.903/
      data pf181/
     1   20.634,   20.634,  20.634,  20.634,  20.634,  20.634,  20.634,
     2   24.348,   24.348,  24.348,  24.348,  24.348,  24.348,  24.348,
     3   29.454,   29.454,  29.454,  29.454,  29.454,  29.453,  29.452,
     4   36.541,   36.540,  36.540,  36.540,  36.540,  36.538,  36.534,
     5    2.022,    2.022,   2.022,   2.022,   2.022,   2.022,   2.022,
     6    2.028,    2.028,   2.028,   2.028,   2.028,   2.028,   2.028,
     7    2.035,    2.035,   2.035,   2.035,   2.035,   2.035,   2.035,
     8    2.043,    2.043,   2.043,   2.043,   2.043,   2.043,   2.043,
     9    2.053,    2.053,   2.053,   2.053,   2.053,   2.053,   2.053/
      data pf182/
     1    2.065,    2.065,   2.065,   2.065,   2.065,   2.065,   2.065,
     2    2.078,    2.078,   2.078,   2.078,   2.078,   2.078,   2.078,
     3    2.093,    2.093,   2.093,   2.093,   2.093,   2.093,   2.093,
     4    2.110,    2.110,   2.110,   2.110,   2.110,   2.110,   2.110,
     5    2.129,    2.129,   2.129,   2.129,   2.129,   2.129,   2.129,
     6    2.151,    2.151,   2.151,   2.151,   2.151,   2.151,   2.151,
     7    2.175,    2.175,   2.175,   2.175,   2.175,   2.175,   2.175,
     8    2.201,    2.201,   2.201,   2.201,   2.201,   2.201,   2.201,
     9    2.230,    2.230,   2.230,   2.230,   2.230,   2.230,   2.230/
      data pf183/
     1    2.262,    2.262,   2.262,   2.262,   2.262,   2.262,   2.262,
     2    2.296,    2.296,   2.296,   2.296,   2.296,   2.296,   2.296,
     3    2.333,    2.333,   2.333,   2.333,   2.333,   2.333,   2.333,
     4    2.372,    2.372,   2.372,   2.372,   2.372,   2.372,   2.372,
     5    2.414,    2.414,   2.414,   2.414,   2.414,   2.414,   2.414,
     6    2.459,    2.459,   2.459,   2.459,   2.459,   2.459,   2.459,
     7    2.530,    2.530,   2.530,   2.530,   2.530,   2.530,   2.530,
     8    2.607,    2.607,   2.607,   2.607,   2.607,   2.607,   2.607,
     9    2.688,    2.688,   2.688,   2.688,   2.688,   2.688,   2.688/
      data pf184/
     1    2.774,    2.774,   2.774,   2.774,   2.774,   2.774,   2.774,
     2    2.864,    2.864,   2.864,   2.864,   2.864,   2.864,   2.864,
     3    2.957,    2.957,   2.957,   2.957,   2.957,   2.957,   2.957,
     4    3.052,    3.052,   3.052,   3.052,   3.052,   3.052,   3.052,
     5    3.151,    3.151,   3.151,   3.151,   3.151,   3.151,   3.151,
     6    3.250,    3.250,   3.250,   3.250,   3.250,   3.250,   3.250,
     7    3.351,    3.351,   3.351,   3.351,   3.351,   3.351,   3.351,
     8    3.520,    3.520,   3.520,   3.520,   3.520,   3.520,   3.520,
     9    3.689,    3.689,   3.689,   3.689,   3.689,   3.689,   3.689/
      data pf185/
     1    3.855,    3.855,   3.855,   3.855,   3.855,   3.855,   3.855,
     2    4.017,    4.017,   4.017,   4.017,   4.017,   4.017,   4.017,
     3    4.173,    4.173,   4.173,   4.173,   4.173,   4.173,   4.173,
     4    4.322,    4.322,   4.322,   4.322,   4.322,   4.322,   4.322,
     5    4.464,    4.464,   4.464,   4.464,   4.464,   4.464,   4.464,
     6    4.598,    4.598,   4.598,   4.598,   4.598,   4.598,   4.598,
     7    4.725,    4.725,   4.725,   4.725,   4.725,   4.725,   4.725,
     8    4.846,    4.846,   4.846,   4.846,   4.846,   4.846,   4.846,
     9    4.965,    4.965,   4.965,   4.965,   4.965,   4.965,   4.965/
      data pf186/
     1    5.085,    5.085,   5.085,   5.085,   5.085,   5.085,   5.085,
     2    5.214,    5.214,   5.214,   5.214,   5.214,   5.214,   5.214,
     3    5.361,    5.361,   5.361,   5.361,   5.361,   5.361,   5.361,
     4    5.540,    5.540,   5.540,   5.540,   5.540,   5.540,   5.540,
     5    5.764,    5.764,   5.764,   5.764,   5.764,   5.764,   5.764,
     6    6.054,    6.054,   6.054,   6.054,   6.054,   6.054,   6.054,
     7    6.430,    6.430,   6.430,   6.430,   6.430,   6.430,   6.430,
     8    6.920,    6.920,   6.920,   6.920,   6.920,   6.920,   6.920,
     9    7.559,    7.559,   7.559,   7.559,   7.559,   7.559,   7.559/
      data pf187/
     1    8.392,    8.392,   8.392,   8.392,   8.392,   8.392,   8.392,
     2    9.482,    9.482,   9.482,   9.482,   9.482,   9.482,   9.482,
     3   10.912,   10.912,  10.912,  10.912,  10.912,  10.912,  10.912,
     4   12.796,   12.796,  12.796,  12.796,  12.796,  12.796,  12.796,
     5   15.289,   15.289,  15.289,  15.289,  15.289,  15.289,  15.289,
     6   18.600,   18.600,  18.600,  18.600,  18.600,  18.600,  18.599,
     7   28.964,   28.964,  28.964,  28.964,  28.964,  28.964,  28.964,
     8   29.653,   29.653,  29.653,  29.653,  29.653,  29.653,  29.653,
     9   30.355,   30.355,  30.355,  30.355,  30.355,  30.355,  30.355/
      data pf188/
     1   31.070,   31.070,  31.070,  31.070,  31.070,  31.070,  31.070,
     2   31.800,   31.800,  31.800,  31.800,  31.800,  31.800,  31.800,
     3   32.546,   32.546,  32.546,  32.546,  32.546,  32.546,  32.546,
     4   33.311,   33.311,  33.311,  33.311,  33.311,  33.311,  33.310,
     5   34.097,   34.097,  34.097,  34.097,  34.097,  34.097,  34.096,
     6   34.909,   34.909,  34.909,  34.909,  34.909,  34.909,  34.907,
     7   35.752,   35.752,  35.752,  35.752,  35.752,  35.752,  35.748,
     8   36.630,   36.630,  36.630,  36.630,  36.630,  36.630,  36.624,
     9   37.550,   37.550,  37.550,  37.550,  37.550,  37.550,  37.541/
      data pf189/
     1   38.521,   38.521,  38.521,  38.521,  38.521,  38.521,  38.506,
     2   39.552,   39.552,  39.552,  39.552,  39.552,  39.552,  39.528,
     3   40.654,   40.654,  40.654,  40.654,  40.654,  40.653,  40.616,
     4   41.839,   41.839,  41.839,  41.839,  41.839,  41.838,  41.781,
     5   43.122,   43.122,  43.122,  43.122,  43.121,  43.120,  43.035,
     6   44.520,   44.520,  44.520,  44.519,  44.519,  44.516,  44.389,
     7   46.051,   46.051,  46.051,  46.050,  46.049,  46.043,  45.858,
     8   47.737,   47.737,  47.737,  47.735,  47.733,  47.723,  47.456,
     9   50.611,   50.611,  50.610,  50.607,  50.601,  50.579,  50.128/
      data pf190/
     1   53.988,   53.987,  53.985,  53.976,  53.962,  53.917,  53.177,
     2   57.987,   57.984,  57.978,  57.958,  57.927,  57.835,  56.657,
     3   62.755,   62.749,  62.736,  62.690,  62.624,  62.447,  60.620,
     4   68.478,   68.466,  68.437,  68.341,  68.207,  67.879,  65.116,
     5   75.387,   75.362,  75.304,  75.111,  74.852,  74.270,  70.189,
     6   83.777,   83.728,  83.614,  83.246,  82.765,  81.770,  75.878,
     7   94.015,   93.923,  93.710,  93.038,  92.180,  90.537,  82.211,
     8  106.560,  106.394, 106.013, 104.833, 103.363, 100.737,  89.209,
     9  121.977,  121.688, 121.031, 119.037, 116.606, 112.538,  96.882/
      data pf191/
     1  155.960,  155.291, 153.784, 149.355, 144.131, 136.206, 111.159,
     2  203.772,  202.360, 199.205, 190.182, 179.852, 165.480, 127.228,
     3  270.211,  267.461, 261.364, 244.355, 225.380, 200.934, 144.936,
     4  360.723,  355.745, 344.776, 314.845, 282.220, 242.947, 164.067,
     5  480.987,  472.537, 454.025, 404.496, 351.604, 291.656, 184.354,
     6  636.365,  622.824, 593.310, 515.716, 434.350, 346.924, 205.502,
     7  831.318,  810.702, 765.974, 650.205, 530.755, 408.348, 227.205,
     8 1068.885, 1038.900, 974.113, 808.736, 640.542, 475.277, 249.159,
     9 1350.316, 1308.446,1218.309, 991.052, 762.864, 546.865, 271.081/
      data pf192/
     1 1674.905, 1618.524,1497.540,1195.859, 896.369, 622.124, 292.715,
     2 2040.034, 1966.531,1809.259,1420.927,1039.299, 699.986, 313.839,
     3 2441.398, 2348.296,2149.603,1663.266,1189.613, 779.369, 334.269,
     4 2873.361, 2758.428,2513.703,1919.352,1345.124, 859.220, 353.857,
     5 3329.382, 3190.712,2896.046,2185.359,1503.617, 938.566, 372.495,
     6 3802.459, 3638.530,3290.825,2457.389,1662.961,1016.538, 390.104,
     7 4285.530, 4095.237,3692.263,2731.665,1821.191,1092.393, 406.640,
     8 4771.823, 4554.478,4094.883,3004.685,1976.565,1165.521, 422.080,
     9 5255.115, 5010.436,4493.705,3273.332,2127.600,1235.444, 436.426/
      data pf193/
     1 5729.917, 5457.992,4884.378,3534.940,2273.087,1301.811, 449.696,
     2 6191.573, 5892.818,5263.250,3787.321,2412.088,1364.386, 461.922,
     3 6636.300, 6311.411,5627.389,4028.763,2543.918,1423.035, 473.146,
     4 7061.173, 6711.069,5974.558,4258.007,2668.125,1477.713, 483.416,
     5 7464.068, 7089.847,6303.169,4474.204,2784.458,1528.443, 492.789,
     6 7843.587, 7446.475,6612.213,4676.869,2892.842,1575.309, 501.319,
     7 8198.964, 7780.273,6901.181,4865.822,2993.341,1618.440, 509.066,
     8 8529.971, 8091.061,7169.988,5041.141,3086.134,1657.997, 516.086,
     9   26.527,   26.527,  26.527,  26.527,  26.527,  26.527,  26.527/
      data pf194/
     1   27.124,   27.124,  27.124,  27.124,  27.124,  27.124,  27.124,
     2   27.742,   27.742,  27.742,  27.742,  27.742,  27.742,  27.742,
     3   28.381,   28.381,  28.381,  28.381,  28.381,  28.381,  28.381,
     4   29.042,   29.042,  29.042,  29.042,  29.042,  29.042,  29.042,
     5   29.727,   29.727,  29.727,  29.727,  29.727,  29.727,  29.727,
     6   30.436,   30.436,  30.436,  30.436,  30.436,  30.436,  30.436,
     7   31.172,   31.172,  31.172,  31.172,  31.172,  31.172,  31.172,
     8   31.936,   31.936,  31.936,  31.936,  31.936,  31.936,  31.936,
     9   32.732,   32.732,  32.732,  32.732,  32.732,  32.732,  32.732/
      data pf195/
     1   33.562,   33.562,  33.562,  33.562,  33.562,  33.562,  33.562,
     2   34.431,   34.431,  34.431,  34.431,  34.431,  34.431,  34.431,
     3   35.342,   35.342,  35.342,  35.342,  35.342,  35.342,  35.342,
     4   36.301,   36.301,  36.301,  36.301,  36.301,  36.301,  36.301,
     5   37.312,   37.312,  37.312,  37.312,  37.312,  37.312,  37.312,
     6   38.382,   38.382,  38.382,  38.382,  38.382,  38.382,  38.382,
     7   39.518,   39.518,  39.518,  39.518,  39.518,  39.518,  39.518,
     8   40.725,   40.725,  40.725,  40.725,  40.725,  40.725,  40.725,
     9   42.012,   42.012,  42.012,  42.012,  42.012,  42.012,  42.012/
      data pf196/
     1   43.387,   43.387,  43.387,  43.387,  43.387,  43.387,  43.387,
     2   45.631,   45.631,  45.631,  45.631,  45.631,  45.631,  45.631,
     3   48.121,   48.121,  48.121,  48.121,  48.121,  48.121,  48.121,
     4   50.887,   50.887,  50.887,  50.887,  50.887,  50.887,  50.887,
     5   53.962,   53.962,  53.962,  53.962,  53.962,  53.962,  53.962,
     6   57.379,   57.379,  57.379,  57.379,  57.379,  57.379,  57.379,
     7   61.172,   61.172,  61.172,  61.172,  61.172,  61.172,  61.172,
     8   65.380,   65.380,  65.380,  65.380,  65.380,  65.380,  65.380,
     9   70.040,   70.040,  70.040,  70.040,  70.039,  70.039,  70.039/
      data pf197/
     1   75.194,   75.194,  75.194,  75.194,  75.194,  75.193,  75.191,
     2   80.890,   80.890,  80.890,  80.890,  80.889,  80.888,  80.883,
     3   91.742,   91.742,  91.741,  91.741,  91.736,  91.729,  91.710,
     4  104.592,  104.590, 104.589, 104.588, 104.563, 104.531, 104.462,
     5  119.955,  119.947, 119.942, 119.937, 119.837, 119.715, 119.496,
     6  138.688,  138.661, 138.641, 138.624, 138.274, 137.877, 137.259,
     7  162.267,  162.180, 162.117, 162.063, 161.004, 159.859, 158.297,
     8  193.182,  192.939, 192.762, 192.612, 189.769, 186.828, 183.251,
     9  235.426,  234.815, 234.373, 234.002, 227.139, 220.324, 212.825/
      data pf198/
     1  294.951,  293.562, 292.561, 291.731, 276.683, 262.263, 247.739,
     2  379.953,  377.068, 374.994, 373.291, 342.993, 314.867, 288.660,
     3  500.805,  495.268, 491.297, 488.070, 431.535, 380.513, 336.130,
     4  669.541,  659.643, 652.560, 646.852, 548.278, 461.514, 390.489,
     5  898.911,  882.298, 870.434, 860.945, 699.154, 559.875, 451.826,
     6 1201.122, 1174.768,1155.980,1141.055, 889.433, 677.055, 519.947,
     7 1586.509, 1546.746,1518.445,1496.097,1123.113, 813.776, 594.375,
     8 2062.332, 2004.965,1964.190,1932.166,1402.448, 969.920, 674.380,
     9 2631.943, 2552.410,2495.950,2451.821,1727.664,1144.515, 759.023/
      data pf199/
     1 3294.384, 3187.969,3112.512,3053.783,2096.908,1335.801, 847.223,
     2 4044.477, 3906.535,3808.817,3733.052,2506.408,1541.374, 937.818,
     3 4873.314, 4699.476,4576.439,4481.367,2950.798,1758.360,1029.634,
     4 5769.029, 5555.398,5404.315,5287.924,3423.548,1983.616,1121.540,
     5 6717.742, 6461.028,6279.603,6140.213,3917.429,2213.916,1212.491,
     6 7704.516, 7402.132,7188.564,7024.873,4424.971,2446.116,1301.561,
     7 8714.251, 8364.358,8117.373,7928.473,4938.856,2677.288,1387.960,
     8 9732.430, 9333.938,9052.787,8838.166,5452.236,2904.817,1471.049,
     910745.700,10298.233,9982.668,9742.185,5958.966,3126.453,1550.328/
      data pf200/
     111742.262,11246.095,10896.32,10630.17, 6453.75, 3340.35, 1625.44,
     2   22.450,   22.450,  22.450,  22.450,  22.450,  22.450,  22.450,
     3   22.668,   22.668,  22.668,  22.668,  22.668,  22.668,  22.668,
     4   22.880,   22.880,  22.880,  22.880,  22.880,  22.880,  22.880,
     5   23.088,   23.088,  23.088,  23.088,  23.088,  23.088,  23.088,
     6   23.292,   23.292,  23.292,  23.292,  23.292,  23.292,  23.292,
     7   23.493,   23.493,  23.493,  23.493,  23.493,  23.493,  23.493,
     8   23.692,   23.692,  23.692,  23.692,  23.692,  23.692,  23.692,
     9   23.889,   23.889,  23.889,  23.889,  23.889,  23.889,  23.889/
      data pf201/
     1   24.087,   24.087,  24.087,  24.087,  24.087,  24.087,  24.087,
     2   24.286,   24.286,  24.286,  24.286,  24.286,  24.286,  24.286,
     3   24.488,   24.488,  24.488,  24.488,  24.488,  24.488,  24.488,
     4   24.695,   24.695,  24.695,  24.695,  24.695,  24.695,  24.695,
     5   24.909,   24.909,  24.909,  24.909,  24.909,  24.909,  24.909,
     6   25.131,   25.131,  25.131,  25.131,  25.131,  25.131,  25.131,
     7   25.365,   25.365,  25.365,  25.365,  25.365,  25.365,  25.365,
     8   25.612,   25.612,  25.612,  25.612,  25.612,  25.612,  25.612,
     9   25.875,   25.875,  25.875,  25.875,  25.875,  25.875,  25.875/
      data pf202/
     1   26.156,   26.156,  26.156,  26.156,  26.156,  26.156,  26.156,
     2   26.458,   26.458,  26.458,  26.458,  26.458,  26.458,  26.458,
     3   26.783,   26.783,  26.783,  26.783,  26.783,  26.783,  26.783,
     4   27.320,   27.320,  27.320,  27.320,  27.320,  27.320,  27.320,
     5   27.924,   27.924,  27.924,  27.924,  27.924,  27.924,  27.924,
     6   28.603,   28.603,  28.603,  28.603,  28.603,  28.603,  28.603,
     7   29.363,   29.363,  29.363,  29.363,  29.363,  29.363,  29.363,
     8   30.212,   30.212,  30.212,  30.212,  30.212,  30.212,  30.212,
     9   31.156,   31.156,  31.156,  31.156,  31.156,  31.156,  31.156/
      data pf203/
     1   32.198,   32.198,  32.198,  32.198,  32.198,  32.198,  32.198,
     2   33.344,   33.344,  33.344,  33.344,  33.344,  33.344,  33.344,
     3   34.597,   34.597,  34.597,  34.597,  34.597,  34.597,  34.597,
     4   35.960,   35.960,  35.960,  35.960,  35.960,  35.960,  35.960,
     5   38.481,   38.481,  38.481,  38.481,  38.481,  38.481,  38.481,
     6   41.326,   41.326,  41.326,  41.326,  41.326,  41.326,  41.326,
     7   44.511,   44.511,  44.511,  44.511,  44.511,  44.511,  44.511,
     8   48.060,   48.060,  48.060,  48.060,  48.060,  48.060,  48.060,
     9   52.010,   52.010,  52.010,  52.010,  52.010,  52.010,  52.010/
      data pf204/
     1   56.423,   56.423,  56.423,  56.423,  56.423,  56.423,  56.423,
     2   61.395,   61.395,  61.395,  61.395,  61.395,  61.395,  61.395,
     3   67.095,   67.095,  67.095,  67.095,  67.095,  67.095,  67.093,
     4   73.831,   73.831,  73.831,  73.830,  73.830,  73.830,  73.822,
     5   82.195,   82.195,  82.195,  82.194,  82.194,  82.192,  82.162,
     6   93.328,   93.328,  93.327,  93.324,  93.321,  93.315,  93.214,
     7  109.315,  109.315, 109.310, 109.300, 109.292, 109.270, 108.977,
     8  133.704,  133.704, 133.691, 133.662, 133.639, 133.578, 132.816,
     9  172.048,  172.047, 172.015, 171.941, 171.881, 171.731, 169.945/
      data pf205/
     1  232.299,  232.297, 232.223, 232.052, 231.915, 231.576, 227.763,
     2  324.880,  324.876, 324.719, 324.359, 324.071, 323.372, 315.872,
     3  462.289,  462.281, 461.974, 461.273, 460.717, 459.385, 445.679,
     4  658.216,  658.201, 657.643, 656.375, 655.373, 653.006, 629.548,
     5  926.273,  926.247, 925.299, 923.147, 921.455, 917.502, 879.632,
     6 1278.553, 1278.513,1276.989,1273.542,1270.843,1264.601,1206.563,
     7 1724.272, 1724.210,1721.885,1716.639,1712.547,1703.168,1618.256,
     8 2268.715, 2268.625,2265.238,2257.610,2251.679,2238.197,2119.001,
     9 2912.655, 2912.528,2907.792,2897.143,2888.889,2870.258,2708.990/
      data pf206/
     1 3652.272, 3652.100,3645.714,3631.377,3620.294,3595.438,3384.303,
     2 4479.541, 4479.316,4470.981,4452.293,4437.882,4405.744,4137.300,
     3 5382.976, 5382.692,5372.122,5348.455,5330.243,5289.835,4957.322,
     4   16.025,   16.025,  16.025,  16.025,  16.025,  16.025,  16.025,
     5   16.208,   16.208,  16.208,  16.208,  16.208,  16.208,  16.208,
     6   16.387,   16.387,  16.387,  16.387,  16.387,  16.387,  16.387,
     7   16.561,   16.561,  16.561,  16.561,  16.561,  16.561,  16.561,
     8   16.732,   16.732,  16.732,  16.732,  16.732,  16.732,  16.732,
     9   16.899,   16.899,  16.899,  16.899,  16.899,  16.899,  16.899/
      data pf207/
     1   17.063,   17.063,  17.063,  17.063,  17.063,  17.063,  17.063,
     2   17.223,   17.223,  17.223,  17.223,  17.223,  17.223,  17.223,
     3   17.381,   17.381,  17.381,  17.381,  17.381,  17.381,  17.381,
     4   17.536,   17.536,  17.536,  17.536,  17.536,  17.536,  17.536,
     5   17.691,   17.691,  17.691,  17.691,  17.691,  17.691,  17.691,
     6   17.844,   17.844,  17.844,  17.844,  17.844,  17.844,  17.844,
     7   17.996,   17.996,  17.996,  17.996,  17.996,  17.996,  17.996,
     8   18.150,   18.150,  18.150,  18.150,  18.150,  18.150,  18.150,
     9   18.304,   18.304,  18.304,  18.304,  18.304,  18.304,  18.304/
      data pf208/
     1   18.460,   18.460,  18.460,  18.460,  18.460,  18.460,  18.460,
     2   18.619,   18.619,  18.619,  18.619,  18.619,  18.619,  18.619,
     3   18.781,   18.781,  18.781,  18.781,  18.781,  18.781,  18.781,
     4   18.948,   18.948,  18.948,  18.948,  18.948,  18.948,  18.948,
     5   19.120,   19.120,  19.120,  19.120,  19.120,  19.120,  19.120,
     6   19.390,   19.390,  19.390,  19.390,  19.390,  19.390,  19.390,
     7   19.676,   19.676,  19.676,  19.676,  19.676,  19.676,  19.676,
     8   19.981,   19.981,  19.981,  19.981,  19.981,  19.981,  19.981,
     9   20.306,   20.306,  20.306,  20.306,  20.306,  20.306,  20.306/
      data pf209/
     1   20.655,   20.655,  20.655,  20.655,  20.655,  20.655,  20.655,
     2   21.027,   21.027,  21.027,  21.027,  21.027,  21.027,  21.027,
     3   21.425,   21.425,  21.425,  21.425,  21.425,  21.425,  21.425,
     4   21.848,   21.848,  21.848,  21.848,  21.848,  21.848,  21.848,
     5   22.296,   22.296,  22.296,  22.296,  22.296,  22.296,  22.296,
     6   22.770,   22.770,  22.770,  22.770,  22.770,  22.770,  22.770,
     7   23.612,   23.612,  23.612,  23.612,  23.612,  23.612,  23.612,
     8   24.515,   24.515,  24.515,  24.515,  24.515,  24.515,  24.515,
     9   25.468,   25.468,  25.468,  25.468,  25.468,  25.468,  25.468/
      data pf210/
     1   26.463,   26.463,  26.463,  26.463,  26.463,  26.463,  26.463,
     2   27.488,   27.488,  27.488,  27.488,  27.488,  27.488,  27.488,
     3   28.535,   28.535,  28.535,  28.535,  28.535,  28.535,  28.535,
     4   29.597,   29.597,  29.597,  29.597,  29.597,  29.597,  29.597,
     5   30.673,   30.673,  30.673,  30.673,  30.673,  30.673,  30.673,
     6   31.769,   31.769,  31.769,  31.769,  31.769,  31.769,  31.769,
     7   32.899,   32.899,  32.899,  32.899,  32.899,  32.899,  32.899,
     8   34.094,   34.094,  34.094,  34.094,  34.094,  34.094,  34.092,
     9   35.409,   35.409,  35.409,  35.409,  35.409,  35.409,  35.399/
      data pf211/
     1   36.952,   36.952,  36.952,  36.952,  36.952,  36.952,  36.910,
     2   38.929,   38.929,  38.929,  38.929,  38.929,  38.928,  38.783,
     3   41.731,   41.730,  41.730,  41.729,  41.729,  41.727,  41.289,
     4   46.057,   46.055,  46.055,  46.053,  46.053,  46.047,  44.874,
     5   53.066,   53.062,  53.062,  53.056,  53.056,  53.043,  50.220,
     6   64.522,   64.512,  64.512,  64.498,  64.498,  64.469,  58.296,
     7   82.878,   82.858,  82.858,  82.829,  82.829,  82.770,  70.369,
     8  111.261,  111.221, 111.221, 111.166, 111.166, 111.053,  87.961,
     9  153.311,  153.238, 153.238, 153.140, 153.140, 152.940, 112.747/
      data pf212/
     1  212.892,  212.769, 212.769, 212.603, 212.603, 212.272, 146.405,
     2  293.715,  293.520, 293.520, 293.256, 293.256, 292.735, 190.434,
     3  398.933,  398.636, 398.636, 398.236, 398.236, 397.459, 245.997,
     4  530.780,  530.350, 530.350, 529.771, 529.771, 528.658, 313.775,
     5  690.322,  689.724, 689.724, 688.919, 688.919, 687.387, 393.899,
     6    7.902,    7.902,   7.902,   7.902,   7.902,   7.902,   7.902,
     7    7.978,    7.978,   7.978,   7.978,   7.978,   7.978,   7.978,
     8    8.052,    8.052,   8.052,   8.052,   8.052,   8.052,   8.052,
     9    8.125,    8.125,   8.125,   8.125,   8.125,   8.125,   8.125/
      data pf213/
     1    8.195,    8.195,   8.195,   8.195,   8.195,   8.195,   8.195,
     2    8.263,    8.263,   8.263,   8.263,   8.263,   8.263,   8.263,
     3    8.329,    8.329,   8.329,   8.329,   8.329,   8.329,   8.329,
     4    8.393,    8.393,   8.393,   8.393,   8.393,   8.393,   8.393,
     5    8.455,    8.455,   8.455,   8.455,   8.455,   8.455,   8.455,
     6    8.515,    8.515,   8.515,   8.515,   8.515,   8.515,   8.515,
     7    8.573,    8.573,   8.573,   8.573,   8.573,   8.573,   8.573,
     8    8.630,    8.630,   8.630,   8.630,   8.630,   8.630,   8.630,
     9    8.684,    8.684,   8.684,   8.684,   8.684,   8.684,   8.684/
      data pf214/
     1    8.736,    8.736,   8.736,   8.736,   8.736,   8.736,   8.736,
     2    8.787,    8.787,   8.787,   8.787,   8.787,   8.787,   8.787,
     3    8.836,    8.836,   8.836,   8.836,   8.836,   8.836,   8.836,
     4    8.883,    8.883,   8.883,   8.883,   8.883,   8.883,   8.883,
     5    8.929,    8.929,   8.929,   8.929,   8.929,   8.929,   8.929,
     6    8.972,    8.972,   8.972,   8.972,   8.972,   8.972,   8.972,
     7    9.015,    9.015,   9.015,   9.015,   9.015,   9.015,   9.015,
     8    9.075,    9.075,   9.075,   9.075,   9.075,   9.075,   9.075,
     9    9.132,    9.132,   9.132,   9.132,   9.132,   9.132,   9.132/
      data pf215/
     1    9.186,    9.186,   9.186,   9.186,   9.186,   9.186,   9.186,
     2    9.237,    9.237,   9.237,   9.237,   9.237,   9.237,   9.237,
     3    9.284,    9.284,   9.284,   9.284,   9.284,   9.284,   9.284,
     4    9.329,    9.329,   9.329,   9.329,   9.329,   9.329,   9.329,
     5    9.372,    9.372,   9.372,   9.372,   9.372,   9.372,   9.372,
     6    9.412,    9.412,   9.412,   9.412,   9.412,   9.412,   9.412,
     7    9.449,    9.449,   9.449,   9.449,   9.449,   9.449,   9.449,
     8    9.484,    9.484,   9.484,   9.484,   9.484,   9.484,   9.484,
     9    9.538,    9.538,   9.538,   9.538,   9.538,   9.538,   9.538/
      data pf216/
     1    9.586,    9.586,   9.586,   9.586,   9.586,   9.586,   9.586,
     2    9.630,    9.630,   9.630,   9.630,   9.630,   9.630,   9.630,
     3    9.669,    9.669,   9.669,   9.669,   9.669,   9.669,   9.669,
     4    9.704,    9.704,   9.704,   9.704,   9.704,   9.704,   9.704,
     5    9.736,    9.736,   9.736,   9.736,   9.736,   9.736,   9.736,
     6    9.764,    9.764,   9.764,   9.764,   9.764,   9.764,   9.764,
     7    9.790,    9.790,   9.790,   9.790,   9.790,   9.790,   9.790,
     8    9.813,    9.813,   9.813,   9.813,   9.813,   9.813,   9.813,
     9    9.835,    9.835,   9.835,   9.835,   9.835,   9.835,   9.835/
      data pf217/
     1    9.857,    9.857,   9.857,   9.857,   9.857,   9.857,   9.857,
     2    9.882,    9.882,   9.882,   9.882,   9.882,   9.882,   9.882,
     3    9.914,    9.914,   9.914,   9.914,   9.914,   9.914,   9.914,
     4    9.966,    9.966,   9.966,   9.966,   9.966,   9.966,   9.966,
     5   10.058,   10.058,  10.058,  10.058,  10.058,  10.058,  10.058,
     6   10.231,   10.231,  10.231,  10.231,  10.231,  10.231,  10.229,
     7   10.557,   10.557,  10.557,  10.557,  10.556,  10.555,  10.549,
     8   11.153,   11.153,  11.152,  11.152,  11.150,  11.148,  11.130,
     9   12.200,   12.200,  12.198,  12.198,  12.194,  12.188,  12.139/
      data pf218/
     1   13.961,   13.961,  13.956,  13.956,  13.947,  13.932,  13.813,
     2   16.788,   16.788,  16.776,  16.776,  16.755,  16.721,  16.463,
     3   21.118,   21.118,  21.093,  21.093,  21.051,  20.980,  20.463,
     4   27.460,   27.460,  27.412,  27.412,  27.332,  27.196,  26.236,
     5   36.356,   36.356,  36.271,  36.271,  36.128,  35.889,  34.220,
     6   48.338,   48.338,  48.195,  48.195,  47.956,  47.557,  44.828,
     7   63.868,   63.868,  63.641,  63.641,  63.264,  62.636,  58.404,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf219/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf220/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf221/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf222/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf223/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     6    1.004,    1.004,   1.004,   1.004,   1.004,   1.004,   1.004,
     7    1.012,    1.012,   1.012,   1.012,   1.012,   1.012,   1.012,
     8    1.031,    1.031,   1.031,   1.031,   1.031,   1.031,   1.031,
     9    1.070,    1.070,   1.070,   1.070,   1.070,   1.070,   1.070/
      data pf224/
     1    1.148,    1.148,   1.148,   1.148,   1.148,   1.148,   1.148,
     2    1.293,    1.293,   1.293,   1.293,   1.293,   1.293,   1.293,
     3    1.553,    1.553,   1.553,   1.553,   1.553,   1.553,   1.553,
     4    2.007,    2.007,   2.007,   2.007,   2.007,   2.007,   2.007,
     5    2.794,    2.794,   2.794,   2.794,   2.794,   2.794,   2.794,
     6    4.157,    4.156,   4.156,   4.156,   4.156,   4.155,   4.155,
     7    6.503,    6.503,   6.502,   6.502,   6.501,   6.499,   6.498,
     8   10.492,   10.491,  10.490,  10.490,  10.486,  10.481,  10.479,
     9   17.115,   17.112,  17.110,  17.110,  17.101,  17.087,  17.084/
      data pf225/
     1    4.010,    4.010,   4.010,   4.010,   4.010,   4.010,   4.010,
     2    4.013,    4.013,   4.013,   4.013,   4.013,   4.013,   4.013,
     3    4.016,    4.016,   4.016,   4.016,   4.016,   4.016,   4.016,
     4    4.020,    4.020,   4.020,   4.020,   4.020,   4.020,   4.020,
     5    4.025,    4.025,   4.025,   4.025,   4.025,   4.025,   4.025,
     6    4.030,    4.030,   4.030,   4.030,   4.030,   4.030,   4.030,
     7    4.036,    4.036,   4.036,   4.036,   4.036,   4.036,   4.036,
     8    4.044,    4.044,   4.044,   4.044,   4.044,   4.044,   4.044,
     9    4.052,    4.052,   4.052,   4.052,   4.052,   4.052,   4.052/
      data pf226/
     1    4.061,    4.061,   4.061,   4.061,   4.061,   4.061,   4.061,
     2    4.071,    4.071,   4.071,   4.071,   4.071,   4.071,   4.071,
     3    4.083,    4.083,   4.083,   4.083,   4.083,   4.083,   4.083,
     4    4.096,    4.096,   4.096,   4.096,   4.096,   4.096,   4.096,
     5    4.110,    4.110,   4.110,   4.110,   4.110,   4.110,   4.110,
     6    4.125,    4.125,   4.125,   4.125,   4.125,   4.125,   4.125,
     7    4.142,    4.142,   4.142,   4.142,   4.142,   4.142,   4.142,
     8    4.160,    4.160,   4.160,   4.160,   4.160,   4.160,   4.160,
     9    4.179,    4.179,   4.179,   4.179,   4.179,   4.179,   4.179/
      data pf227/
     1    4.200,    4.200,   4.200,   4.200,   4.200,   4.200,   4.200,
     2    4.221,    4.221,   4.221,   4.221,   4.221,   4.221,   4.221,
     3    4.256,    4.256,   4.256,   4.256,   4.256,   4.256,   4.256,
     4    4.294,    4.294,   4.294,   4.294,   4.294,   4.294,   4.294,
     5    4.334,    4.334,   4.334,   4.334,   4.334,   4.334,   4.334,
     6    4.377,    4.377,   4.377,   4.377,   4.377,   4.377,   4.377,
     7    4.421,    4.421,   4.421,   4.421,   4.421,   4.421,   4.421,
     8    4.467,    4.467,   4.467,   4.467,   4.467,   4.467,   4.467,
     9    4.515,    4.515,   4.515,   4.515,   4.515,   4.515,   4.515/
      data pf228/
     1    4.564,    4.564,   4.564,   4.564,   4.564,   4.564,   4.564,
     2    4.613,    4.613,   4.613,   4.613,   4.613,   4.613,   4.613,
     3    4.664,    4.664,   4.664,   4.664,   4.664,   4.664,   4.664,
     4    4.748,    4.748,   4.748,   4.748,   4.748,   4.748,   4.748,
     5    4.833,    4.833,   4.833,   4.833,   4.833,   4.833,   4.833,
     6    4.916,    4.916,   4.916,   4.916,   4.916,   4.916,   4.916,
     7    4.997,    4.997,   4.997,   4.997,   4.997,   4.997,   4.997,
     8    5.075,    5.075,   5.075,   5.075,   5.075,   5.075,   5.075,
     9    5.151,    5.151,   5.151,   5.151,   5.151,   5.151,   5.151/
      data pf229/
     1    5.222,    5.222,   5.222,   5.222,   5.222,   5.222,   5.222,
     2    5.289,    5.289,   5.289,   5.289,   5.289,   5.289,   5.289,
     3    5.352,    5.352,   5.352,   5.352,   5.352,   5.352,   5.352,
     4    5.411,    5.411,   5.411,   5.411,   5.411,   5.411,   5.411,
     5    5.466,    5.466,   5.466,   5.466,   5.466,   5.466,   5.466,
     6    5.517,    5.517,   5.517,   5.517,   5.517,   5.517,   5.517,
     7    5.567,    5.567,   5.567,   5.567,   5.567,   5.567,   5.567,
     8    5.618,    5.618,   5.618,   5.618,   5.618,   5.618,   5.618,
     9    5.677,    5.677,   5.677,   5.677,   5.677,   5.677,   5.677/
      data pf230/
     1    5.758,    5.758,   5.758,   5.758,   5.758,   5.758,   5.758,
     2    5.885,    5.885,   5.885,   5.885,   5.885,   5.885,   5.885,
     3    6.096,    6.096,   6.096,   6.096,   6.096,   6.096,   6.096,
     4    6.454,    6.454,   6.454,   6.454,   6.454,   6.454,   6.454,
     5    7.050,    7.050,   7.050,   7.050,   7.050,   7.050,   7.050,
     6    8.026,    8.026,   8.026,   8.026,   8.026,   8.026,   8.026,
     7    9.607,    9.607,   9.607,   9.607,   9.607,   9.607,   9.606,
     8   12.154,   12.154,  12.154,  12.154,  12.154,  12.153,  12.151,
     9   16.263,   16.263,  16.263,  16.262,  16.261,  16.258,  16.252/
      data pf231/
     1   22.903,   22.903,  22.901,  22.900,  22.896,  22.885,  22.868,
     2   33.601,   33.600,  33.594,  33.592,  33.579,  33.547,  33.499,
     3    5.053,    5.053,   5.053,   5.053,   5.053,   5.053,   5.053,
     4    5.065,    5.065,   5.065,   5.065,   5.065,   5.065,   5.065,
     5    5.077,    5.077,   5.077,   5.077,   5.077,   5.077,   5.077,
     6    5.092,    5.092,   5.092,   5.092,   5.092,   5.092,   5.092,
     7    5.109,    5.109,   5.109,   5.109,   5.109,   5.109,   5.109,
     8    5.128,    5.128,   5.128,   5.128,   5.128,   5.128,   5.128,
     9    5.149,    5.149,   5.149,   5.149,   5.149,   5.149,   5.149/
      data pf232/
     1    5.173,    5.173,   5.173,   5.173,   5.173,   5.173,   5.173,
     2    5.198,    5.198,   5.198,   5.198,   5.198,   5.198,   5.198,
     3    5.227,    5.227,   5.227,   5.227,   5.227,   5.227,   5.227,
     4    5.258,    5.258,   5.258,   5.258,   5.258,   5.258,   5.258,
     5    5.291,    5.291,   5.291,   5.291,   5.291,   5.291,   5.291,
     6    5.327,    5.327,   5.327,   5.327,   5.327,   5.327,   5.327,
     7    5.366,    5.366,   5.366,   5.366,   5.366,   5.366,   5.366,
     8    5.407,    5.407,   5.407,   5.407,   5.407,   5.407,   5.407,
     9    5.451,    5.451,   5.451,   5.451,   5.451,   5.451,   5.451/
      data pf233/
     1    5.497,    5.497,   5.497,   5.497,   5.497,   5.497,   5.497,
     2    5.546,    5.546,   5.546,   5.546,   5.546,   5.546,   5.546,
     3    5.597,    5.597,   5.597,   5.597,   5.597,   5.597,   5.597,
     4    5.650,    5.650,   5.650,   5.650,   5.650,   5.650,   5.650,
     5    5.735,    5.735,   5.735,   5.735,   5.735,   5.735,   5.735,
     6    5.824,    5.824,   5.824,   5.824,   5.824,   5.824,   5.824,
     7    5.918,    5.918,   5.918,   5.918,   5.918,   5.918,   5.918,
     8    6.017,    6.017,   6.017,   6.017,   6.017,   6.017,   6.017,
     9    6.119,    6.119,   6.119,   6.119,   6.119,   6.119,   6.119/
      data pf234/
     1    6.227,    6.227,   6.227,   6.227,   6.227,   6.227,   6.227,
     2    6.339,    6.339,   6.339,   6.339,   6.339,   6.339,   6.339,
     3    6.455,    6.455,   6.455,   6.455,   6.455,   6.455,   6.455,
     4    6.576,    6.576,   6.576,   6.576,   6.576,   6.576,   6.576,
     5    6.702,    6.702,   6.702,   6.702,   6.702,   6.702,   6.702,
     6    6.923,    6.923,   6.923,   6.923,   6.923,   6.923,   6.923,
     7    7.158,    7.158,   7.158,   7.158,   7.158,   7.158,   7.158,
     8    7.408,    7.408,   7.408,   7.408,   7.408,   7.408,   7.408,
     9    7.674,    7.674,   7.674,   7.674,   7.674,   7.674,   7.674/
      data pf235/
     1    7.953,    7.953,   7.953,   7.953,   7.953,   7.953,   7.953,
     2    8.247,    8.247,   8.247,   8.247,   8.247,   8.247,   8.247,
     3    8.551,    8.551,   8.551,   8.551,   8.551,   8.551,   8.551,
     4    8.866,    8.866,   8.866,   8.866,   8.866,   8.866,   8.866,
     5    9.187,    9.187,   9.187,   9.187,   9.187,   9.187,   9.187,
     6    9.514,    9.514,   9.514,   9.514,   9.514,   9.514,   9.514,
     7    9.842,    9.842,   9.842,   9.842,   9.842,   9.842,   9.842,
     8   10.170,   10.170,  10.170,  10.170,  10.170,  10.170,  10.170,
     9   10.499,   10.499,  10.499,  10.499,  10.499,  10.499,  10.499/
      data pf236/
     1   10.829,   10.829,  10.829,  10.829,  10.829,  10.829,  10.829,
     2   11.169,   11.169,  11.169,  11.169,  11.169,  11.169,  11.169,
     3   11.534,   11.534,  11.534,  11.534,  11.534,  11.534,  11.534,
     4   11.953,   11.953,  11.953,  11.953,  11.953,  11.953,  11.953,
     5   12.472,   12.472,  12.472,  12.472,  12.472,  12.472,  12.472,
     6   13.169,   13.169,  13.169,  13.169,  13.169,  13.169,  13.169,
     7   14.156,   14.156,  14.156,  14.156,  14.156,  14.156,  14.156,
     8   15.603,   15.603,  15.603,  15.603,  15.603,  15.603,  15.603,
     9   17.762,   17.762,  17.762,  17.762,  17.762,  17.762,  17.762/
      data pf237/
     1   21.013,   21.013,  21.013,  21.013,  21.013,  21.013,  21.013,
     2   25.942,   25.942,  25.942,  25.942,  25.941,  25.941,  25.940,
     3   33.459,   33.459,  33.459,  33.458,  33.457,  33.456,  33.452,
     4   44.982,   44.982,  44.982,  44.979,  44.975,  44.969,  44.958,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf238/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf239/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     8    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     9    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002/
      data pf240/
     1    4.004,    4.004,   4.004,   4.004,   4.004,   4.004,   4.004,
     2    4.007,    4.007,   4.007,   4.007,   4.007,   4.007,   4.007,
     3    4.012,    4.012,   4.012,   4.012,   4.012,   4.012,   4.012,
     4    4.018,    4.018,   4.018,   4.018,   4.018,   4.018,   4.018,
     5    4.028,    4.028,   4.028,   4.028,   4.028,   4.028,   4.028,
     6    4.042,    4.042,   4.042,   4.042,   4.042,   4.042,   4.042,
     7    4.060,    4.060,   4.060,   4.060,   4.060,   4.060,   4.060,
     8    4.106,    4.106,   4.106,   4.106,   4.106,   4.106,   4.106,
     9    4.176,    4.176,   4.176,   4.176,   4.176,   4.176,   4.176/
      data pf241/
     1    4.278,    4.278,   4.278,   4.278,   4.278,   4.278,   4.278,
     2    4.418,    4.418,   4.418,   4.418,   4.418,   4.418,   4.418,
     3    4.604,    4.604,   4.604,   4.604,   4.604,   4.604,   4.604,
     4    4.841,    4.841,   4.841,   4.841,   4.841,   4.841,   4.841,
     5    5.134,    5.134,   5.134,   5.134,   5.134,   5.134,   5.134,
     6    5.485,    5.485,   5.485,   5.485,   5.485,   5.485,   5.485,
     7    5.894,    5.894,   5.894,   5.894,   5.894,   5.894,   5.894,
     8    6.358,    6.358,   6.358,   6.358,   6.358,   6.358,   6.358,
     9    6.874,    6.874,   6.874,   6.874,   6.874,   6.874,   6.874/
      data pf242/
     1    7.436,    7.436,   7.436,   7.436,   7.436,   7.436,   7.436,
     2    8.040,    8.040,   8.040,   8.040,   8.040,   8.040,   8.040,
     3    8.682,    8.682,   8.682,   8.682,   8.682,   8.682,   8.682,
     4    9.363,    9.363,   9.363,   9.363,   9.363,   9.363,   9.363,
     5   10.088,   10.088,  10.088,  10.088,  10.088,  10.088,  10.088,
     6   10.875,   10.875,  10.875,  10.875,  10.875,  10.875,  10.875,
     7   11.754,   11.754,  11.754,  11.754,  11.754,  11.754,  11.754,
     8   12.778,   12.778,  12.778,  12.778,  12.778,  12.778,  12.778,
     9   14.025,   14.025,  14.025,  14.025,  14.025,  14.025,  14.025/
      data pf243/
     1   15.611,   15.611,  15.611,  15.611,  15.611,  15.611,  15.611,
     2   17.707,   17.707,  17.707,  17.707,  17.707,  17.707,  17.707,
     3   20.556,   20.556,  20.556,  20.556,  20.556,  20.556,  20.556,
     4   24.515,   24.515,  24.515,  24.515,  24.515,  24.515,  24.515,
     5   30.111,   30.111,  30.111,  30.111,  30.111,  30.111,  30.111,
     6   38.133,   38.133,  38.133,  38.133,  38.133,  38.133,  38.132,
     7    1.176,    1.176,   1.176,   1.176,   1.176,   1.176,   1.176,
     8    1.202,    1.202,   1.202,   1.202,   1.202,   1.202,   1.202,
     9    1.231,    1.231,   1.231,   1.231,   1.231,   1.231,   1.231/
      data pf244/
     1    1.262,    1.262,   1.262,   1.262,   1.262,   1.262,   1.262,
     2    1.296,    1.296,   1.296,   1.296,   1.296,   1.296,   1.296,
     3    1.334,    1.334,   1.334,   1.334,   1.334,   1.334,   1.334,
     4    1.374,    1.374,   1.374,   1.374,   1.374,   1.374,   1.374,
     5    1.418,    1.418,   1.418,   1.418,   1.418,   1.418,   1.418,
     6    1.466,    1.466,   1.466,   1.466,   1.466,   1.466,   1.466,
     7    1.517,    1.517,   1.517,   1.517,   1.517,   1.517,   1.517,
     8    1.571,    1.571,   1.571,   1.571,   1.571,   1.571,   1.571,
     9    1.630,    1.630,   1.630,   1.630,   1.630,   1.630,   1.630/
      data pf245/
     1    1.692,    1.692,   1.692,   1.692,   1.692,   1.692,   1.692,
     2    1.758,    1.758,   1.758,   1.758,   1.758,   1.758,   1.758,
     3    1.828,    1.828,   1.828,   1.828,   1.828,   1.828,   1.828,
     4    1.903,    1.903,   1.903,   1.903,   1.903,   1.903,   1.903,
     5    1.981,    1.981,   1.981,   1.981,   1.981,   1.981,   1.981,
     6    2.063,    2.063,   2.063,   2.063,   2.063,   2.063,   2.063,
     7    2.149,    2.149,   2.149,   2.149,   2.149,   2.149,   2.149,
     8    2.239,    2.239,   2.239,   2.239,   2.239,   2.239,   2.239,
     9    2.380,    2.380,   2.380,   2.380,   2.380,   2.380,   2.380/
      data pf246/
     1    2.531,    2.531,   2.531,   2.531,   2.531,   2.531,   2.531,
     2    2.689,    2.689,   2.689,   2.689,   2.689,   2.689,   2.689,
     3    2.854,    2.854,   2.854,   2.854,   2.854,   2.854,   2.854,
     4    3.027,    3.027,   3.027,   3.027,   3.027,   3.027,   3.027,
     5    3.206,    3.206,   3.206,   3.206,   3.206,   3.206,   3.206,
     6    3.391,    3.391,   3.391,   3.391,   3.391,   3.391,   3.391,
     7    3.582,    3.582,   3.582,   3.582,   3.582,   3.582,   3.582,
     8    3.778,    3.778,   3.778,   3.778,   3.778,   3.778,   3.778,
     9    3.980,    3.980,   3.980,   3.980,   3.980,   3.980,   3.980/
      data pf247/
     1    4.327,    4.327,   4.327,   4.327,   4.327,   4.327,   4.327,
     2    4.686,    4.686,   4.686,   4.686,   4.686,   4.686,   4.686,
     3    5.058,    5.058,   5.058,   5.058,   5.058,   5.058,   5.058,
     4    5.442,    5.442,   5.442,   5.442,   5.442,   5.442,   5.442,
     5    5.837,    5.837,   5.837,   5.837,   5.837,   5.837,   5.837,
     6    6.241,    6.241,   6.241,   6.241,   6.241,   6.241,   6.241,
     7    6.654,    6.654,   6.654,   6.654,   6.654,   6.654,   6.654,
     8    7.074,    7.074,   7.074,   7.074,   7.074,   7.074,   7.074,
     9    7.499,    7.499,   7.499,   7.499,   7.499,   7.499,   7.499/
      data pf248/
     1    7.927,    7.927,   7.927,   7.927,   7.927,   7.927,   7.927,
     2    8.358,    8.358,   8.358,   8.358,   8.358,   8.358,   8.358,
     3    8.792,    8.792,   8.792,   8.792,   8.792,   8.792,   8.792,
     4    9.230,    9.230,   9.230,   9.230,   9.230,   9.230,   9.230,
     5    9.679,    9.679,   9.679,   9.679,   9.679,   9.679,   9.679,
     6   10.148,   10.148,  10.148,  10.148,  10.148,  10.148,  10.148,
     7   10.651,   10.651,  10.651,  10.651,  10.651,  10.651,  10.651,
     8   11.210,   11.210,  11.210,  11.210,  11.210,  11.210,  11.210,
     9   11.856,   11.856,  11.856,  11.856,  11.856,  11.856,  11.856/
      data pf249/
     1   12.631,   12.631,  12.631,  12.631,  12.631,  12.631,  12.631,
     2   13.593,   13.593,  13.593,  13.593,  13.593,  13.593,  13.593,
     3   14.822,   14.822,  14.822,  14.822,  14.822,  14.822,  14.822,
     4   16.428,   16.428,  16.428,  16.428,  16.428,  16.428,  16.428,
     5   18.563,   18.563,  18.563,  18.563,  18.563,  18.563,  18.563,
     6   21.438,   21.438,  21.438,  21.438,  21.438,  21.438,  21.438,
     7   25.346,   25.346,  25.346,  25.346,  25.346,  25.346,  25.346,
     8   30.698,   30.698,  30.698,  30.698,  30.698,  30.698,  30.698,
     9    7.123,    7.123,   7.123,   7.123,   7.123,   7.123,   7.123/
      data pf250/
     1    7.157,    7.157,   7.157,   7.157,   7.157,   7.157,   7.157,
     2    7.199,    7.199,   7.199,   7.199,   7.199,   7.199,   7.199,
     3    7.249,    7.249,   7.249,   7.249,   7.249,   7.249,   7.249,
     4    7.310,    7.310,   7.310,   7.310,   7.310,   7.310,   7.309,
     5    7.380,    7.380,   7.380,   7.380,   7.380,   7.380,   7.380,
     6    7.463,    7.463,   7.463,   7.463,   7.463,   7.463,   7.463,
     7    7.560,    7.560,   7.560,   7.560,   7.560,   7.560,   7.559,
     8    7.671,    7.671,   7.671,   7.671,   7.671,   7.671,   7.669,
     9    7.798,    7.798,   7.798,   7.798,   7.798,   7.798,   7.795/
      data pf251/
     1    7.943,    7.943,   7.943,   7.943,   7.943,   7.943,   7.938,
     2    8.107,    8.107,   8.107,   8.107,   8.107,   8.107,   8.099,
     3    8.293,    8.293,   8.293,   8.293,   8.293,   8.293,   8.278,
     4    8.501,    8.501,   8.501,   8.501,   8.501,   8.501,   8.479,
     5    8.735,    8.735,   8.735,   8.735,   8.735,   8.735,   8.700,
     6    8.998,    8.998,   8.998,   8.998,   8.998,   8.998,   8.945,
     7    9.292,    9.292,   9.292,   9.292,   9.292,   9.292,   9.213,
     8    9.623,    9.623,   9.623,   9.623,   9.623,   9.622,   9.506,
     9    9.995,    9.995,   9.995,   9.995,   9.994,   9.992,   9.826/
      data pf252/
     1   10.414,   10.414,  10.414,  10.414,  10.412,  10.409,  10.172,
     2   11.148,   11.148,  11.148,  11.148,  11.145,  11.138,  10.747,
     3   12.040,   12.040,  12.039,  12.038,  12.032,  12.017,  11.389,
     4   13.131,   13.131,  13.129,  13.127,  13.112,  13.083,  12.105,
     5   14.480,   14.478,  14.475,  14.469,  14.438,  14.381,  12.898,
     6   16.157,   16.154,  16.146,  16.135,  16.073,  15.966,  13.773,
     7   18.256,   18.250,  18.234,  18.211,  18.091,  17.899,  14.733,
     8   20.893,   20.880,  20.849,  20.805,  20.583,  20.253,  15.782,
     9   24.210,   24.187,  24.128,  24.048,  23.654,  23.104,  16.922/
      data pf253/
     1   28.384,   28.342,  28.236,  28.094,  27.422,  26.536,  18.155,
     2   33.621,   33.549,  33.365,  33.123,  32.017,  30.634,  19.482,
     3   45.393,   45.226,  44.803,  44.262,  41.897,  39.176,  21.900,
     4   62.161,   61.807,  60.920,  59.810,  55.151,  50.172,  24.566,
     5   85.505,   84.813,  83.099,  80.991,  72.467,  63.928,  27.460,
     6  117.147,  115.892, 112.806, 109.073,  94.466,  80.650,  30.555,
     7  158.815,  156.679, 151.467, 145.256, 121.644, 100.420,  33.812,
     8  212.068,  208.639, 200.326, 190.546, 154.315, 123.189,  37.193,
     9  278.133,  272.905, 260.299, 245.642, 192.573, 148.775,  40.653/
      data pf254/
     1  357.757,  350.143, 331.876, 310.855, 236.276, 176.883,  44.148,
     2  451.119,  440.473, 415.049, 386.061, 285.054, 207.124,  47.638,
     3  557.795,  543.443, 509.306, 470.702, 338.335, 239.048,  51.082,
     4  676.789,  658.058, 613.669, 563.837, 395.393, 272.170,  54.449,
     5  806.615,  782.869, 726.773, 664.207, 455.393, 306.002,  57.708,
     6  945.415,  916.078, 846.969, 770.336, 517.447, 340.073,  60.837,
     7 1091.092, 1055.670, 972.439, 880.623, 580.664, 373.950,  63.818,
     8 1241.441, 1199.539,1101.308, 993.441, 644.187, 407.250,  66.639,
     9 1394.271, 1345.602,1231.737,1107.214, 707.233, 439.645,  69.291/
      data pf255/
     1 1547.507, 1491.892,1362.008,1220.483, 769.109, 470.869,  71.771,
     2 1699.264, 1636.625,1490.574,1331.950, 829.228, 500.716,  74.078,
     3 1847.894, 1778.252,1616.103,1440.505, 887.111, 529.034,  76.214,
     4 1992.015, 1915.474,1737.489,1545.237, 942.390, 555.723,  78.185,
     5 2130.518, 2047.254,1853.855,1645.434, 994.797, 580.726,  79.996,
     6 2262.556, 2172.804,1964.547,1740.572,1044.155, 604.024,  81.655,
     7 2387.530, 2291.570,2069.111,1830.298,1090.370, 625.631,  83.170,
     8 2505.057, 2403.204,2167.272,1914.408,1133.414, 645.584,  84.549,
     9 2614.947, 2507.537,2258.912,1992.830,1173.316, 663.939,  85.803/
      data pf256/
     1 2717.167, 2604.550,2344.038,2065.593,1210.150, 680.766,  86.941,
     2    6.006,    6.006,   6.006,   6.006,   6.006,   6.006,   6.006,
     3    6.010,    6.010,   6.010,   6.010,   6.010,   6.010,   6.010,
     4    6.014,    6.014,   6.014,   6.014,   6.014,   6.014,   6.014,
     5    6.019,    6.019,   6.019,   6.019,   6.019,   6.019,   6.019,
     6    6.027,    6.027,   6.027,   6.027,   6.027,   6.027,   6.027,
     7    6.038,    6.038,   6.038,   6.038,   6.038,   6.038,   6.038,
     8    6.051,    6.051,   6.051,   6.051,   6.051,   6.051,   6.051,
     9    6.069,    6.069,   6.069,   6.069,   6.069,   6.069,   6.069/
      data pf257/
     1    6.091,    6.091,   6.091,   6.091,   6.091,   6.091,   6.091,
     2    6.119,    6.119,   6.119,   6.119,   6.119,   6.119,   6.119,
     3    6.155,    6.155,   6.155,   6.155,   6.155,   6.155,   6.155,
     4    6.200,    6.200,   6.200,   6.200,   6.200,   6.200,   6.200,
     5    6.254,    6.254,   6.254,   6.254,   6.254,   6.254,   6.254,
     6    6.321,    6.321,   6.321,   6.321,   6.321,   6.321,   6.321,
     7    6.402,    6.402,   6.402,   6.402,   6.402,   6.402,   6.402,
     8    6.500,    6.500,   6.500,   6.500,   6.500,   6.500,   6.500,
     9    6.618,    6.618,   6.618,   6.618,   6.618,   6.618,   6.618/
      data pf258/
     1    6.758,    6.758,   6.758,   6.758,   6.758,   6.758,   6.758,
     2    6.924,    6.924,   6.924,   6.924,   6.924,   6.924,   6.924,
     3    7.119,    7.119,   7.119,   7.119,   7.119,   7.119,   7.119,
     4    7.479,    7.479,   7.479,   7.479,   7.479,   7.479,   7.479,
     5    7.933,    7.933,   7.933,   7.933,   7.933,   7.933,   7.933,
     6    8.504,    8.504,   8.504,   8.504,   8.504,   8.504,   8.504,
     7    9.215,    9.215,   9.215,   9.215,   9.215,   9.215,   9.215,
     8   10.096,   10.096,  10.096,  10.096,  10.096,  10.096,  10.096,
     9   11.181,   11.181,  11.181,  11.181,  11.181,  11.181,  11.181/
      data pf259/
     1   12.513,   12.513,  12.513,  12.513,  12.513,  12.513,  12.513,
     2   14.136,   14.136,  14.136,  14.136,  14.136,  14.136,  14.136,
     3   16.105,   16.105,  16.105,  16.105,  16.105,  16.105,  16.105,
     4   18.483,   18.483,  18.483,  18.482,  18.482,  18.482,  18.482,
     5   23.546,   23.546,  23.546,  23.546,  23.546,  23.545,  23.541,
     6   30.334,   30.334,  30.333,  30.333,  30.331,  30.327,  30.309,
     7   39.351,   39.351,  39.350,  39.349,  39.340,  39.321,  39.253,
     8   51.284,   51.283,  51.277,  51.273,  51.236,  51.166,  50.941,
     9   67.117,   67.112,  67.093,  67.077,  66.948,  66.714,  66.060/
      data pf260/
     1   88.325,   88.308,  88.249,  88.197,  87.804,  87.119,  85.423,
     2  117.134,  117.085, 116.919, 116.777, 115.715, 113.930, 109.961,
     3  156.815,  156.694, 156.277, 155.923, 153.348, 149.158, 140.677,
     4  211.945,  211.668, 210.724, 209.929, 204.257, 195.290, 178.592,
     5  288.513,  287.935, 285.980, 284.341, 272.876, 255.207, 224.644,
     6  393.796,  392.683, 388.941, 385.821, 364.351, 332.009, 279.596,
     7  535.941,  533.947, 527.272, 521.733, 484.180, 428.742, 343.941,
     8  723.292,  719.938, 708.758, 699.518, 637.709, 548.081, 417.828,
     9  963.543,  958.211, 940.507, 925.928, 829.560, 692.026, 501.025/
      data pf261/
     1 1262.881, 1254.821,1228.152,1206.264,1063.095, 861.645, 592.923,
     2 1625.255, 1613.609,1575.186,1543.743,1340.007,1056.922, 692.566,
     3 2051.899, 2035.730,1982.529,1939.106,1660.089,1276.726, 798.722,
     4 2541.153, 2519.491,2448.388,2390.489,2021.221,1518.888, 909.960,
     5 3088.595, 3060.483,2968.406,2893.582,2419.537,1780.368,1024.743,
     6 3687.426, 3651.963,3536.029,3441.993,2849.748,2057.487,1141.511,
     7 4329.026, 4285.406,4143.046,4027.765,3305.545,2346.179,1258.757,
     8 5003.596, 4951.136,4780.188,4641.959,3780.035,2642.238,1375.085,
     9 5700.796, 5638.959,5437.725,5275.221,4266.161,2941.538,1489.255/
      data pf262/
     1 6410.329, 6338.731,6106.012,5918.300,4757.061,3240.214,1600.207,
     2 7122.420, 7040.831,6775.921,6562.465,5246.354,3534.790,1707.073,
     3 7828.173, 7736.510,7439.175,7199.813,5728.346,3822.262,1809.177,
     4   19.811,   19.811,  19.811,  19.811,  19.811,  19.811,  19.811,
     5   20.011,   20.011,  20.011,  20.011,  20.011,  20.011,  20.011,
     6   20.206,   20.206,  20.206,  20.206,  20.206,  20.206,  20.206,
     7   20.394,   20.394,  20.394,  20.394,  20.394,  20.394,  20.394,
     8   20.577,   20.577,  20.577,  20.577,  20.577,  20.577,  20.577,
     9   20.754,   20.754,  20.754,  20.754,  20.754,  20.754,  20.754/
      data pf263/
     1   20.926,   20.926,  20.926,  20.926,  20.926,  20.926,  20.926,
     2   21.093,   21.093,  21.093,  21.093,  21.093,  21.093,  21.093,
     3   21.256,   21.256,  21.256,  21.256,  21.256,  21.256,  21.256,
     4   21.415,   21.415,  21.415,  21.415,  21.415,  21.415,  21.415,
     5   21.572,   21.572,  21.572,  21.572,  21.572,  21.572,  21.572,
     6   21.727,   21.727,  21.727,  21.727,  21.727,  21.727,  21.727,
     7   21.882,   21.882,  21.882,  21.882,  21.882,  21.882,  21.882,
     8   22.038,   22.038,  22.038,  22.038,  22.038,  22.038,  22.038,
     9   22.196,   22.196,  22.196,  22.196,  22.196,  22.196,  22.196/
      data pf264/
     1   22.360,   22.360,  22.360,  22.360,  22.360,  22.360,  22.360,
     2   22.531,   22.531,  22.531,  22.531,  22.531,  22.531,  22.531,
     3   22.711,   22.711,  22.711,  22.711,  22.711,  22.711,  22.711,
     4   22.905,   22.905,  22.905,  22.905,  22.905,  22.905,  22.905,
     5   23.115,   23.115,  23.115,  23.115,  23.115,  23.115,  23.115,
     6   23.468,   23.468,  23.468,  23.468,  23.468,  23.468,  23.468,
     7   23.878,   23.878,  23.878,  23.878,  23.878,  23.878,  23.878,
     8   24.361,   24.361,  24.361,  24.361,  24.361,  24.361,  24.361,
     9   24.932,   24.932,  24.932,  24.932,  24.932,  24.932,  24.932/
      data pf265/
     1   25.607,   25.607,  25.607,  25.607,  25.607,  25.607,  25.607,
     2   26.403,   26.403,  26.403,  26.403,  26.403,  26.403,  26.403,
     3   27.337,   27.337,  27.337,  27.337,  27.337,  27.337,  27.337,
     4   28.424,   28.424,  28.424,  28.424,  28.424,  28.424,  28.424,
     5   29.681,   29.681,  29.681,  29.681,  29.681,  29.681,  29.681,
     6   31.123,   31.123,  31.123,  31.123,  31.123,  31.123,  31.123,
     7   33.975,   33.975,  33.975,  33.975,  33.975,  33.975,  33.975,
     8   37.444,   37.444,  37.444,  37.444,  37.444,  37.444,  37.444,
     9   41.593,   41.593,  41.593,  41.593,  41.593,  41.593,  41.593/
      data pf266/
     1   46.493,   46.493,  46.493,  46.493,  46.493,  46.493,  46.493,
     2   52.230,   52.230,  52.230,  52.230,  52.230,  52.230,  52.230,
     3   58.922,   58.922,  58.922,  58.922,  58.922,  58.921,  58.921,
     4   66.740,   66.740,  66.740,  66.740,  66.739,  66.737,  66.733,
     5   75.952,   75.952,  75.951,  75.950,  75.946,  75.933,  75.914,
     6   87.001,   86.999,  86.997,  86.991,  86.974,  86.917,  86.839,
     7  100.669,  100.662, 100.656, 100.632, 100.567, 100.351, 100.077,
     8  118.374,  118.351, 118.328, 118.249, 118.030, 117.331, 116.488,
     9  142.640,  142.572, 142.502, 142.267, 141.625, 139.630, 137.332/
      data pf267/
     1  177.738,  177.556, 177.370, 176.751, 175.073, 169.993, 164.377,
     2  230.398,  229.961, 229.518, 228.050, 224.102, 212.416, 199.958,
     3  310.396,  309.446, 308.483, 305.316, 296.852, 272.296, 246.951,
     4  430.781,  428.882, 426.960, 420.673, 403.971, 356.375, 308.638,
     5  607.516,  603.995, 600.437, 588.854, 558.242, 472.393, 388.456,
     6  858.470,  852.368, 846.205, 826.240, 773.710, 628.481, 489.667,
     7 1201.849, 1191.885,1181.832,1149.396,1064.397, 832.366, 615.004,
     8 1654.310, 1638.886,1623.334,1573.347,1442.822,1090.522, 766.348,
     9 2229.096, 2206.327,2183.387,2109.890,1918.590,1407.425, 944.499/
      data pf268/
     1 2934.518, 2902.301,2869.861,2766.233,2497.277,1785.028,1149.065,
     2 3773.008, 3729.110,3684.934,3544.182,3179.798,2222.521,1378.478,
     3 4740.848, 4683.014,4624.842,4439.926,3962.293,2716.399,1630.131,
     4 5828.546, 5754.602,5680.259,5444.426,4836.504,3260.778,1900.585,
     5 7021.723, 6929.676,6837.166,6544.247,5790.527,3847.903,2185.835,
     6   19.764,   19.764,  19.764,  19.764,  19.764,  19.764,  19.764,
     7   20.053,   20.053,  20.053,  20.053,  20.053,  20.053,  20.053,
     8   20.334,   20.334,  20.334,  20.334,  20.334,  20.334,  20.334,
     9   20.610,   20.610,  20.610,  20.610,  20.610,  20.610,  20.610/
      data pf269/
     1   20.879,   20.879,  20.879,  20.879,  20.879,  20.879,  20.879,
     2   21.142,   21.142,  21.142,  21.142,  21.142,  21.142,  21.142,
     3   21.399,   21.399,  21.399,  21.399,  21.399,  21.399,  21.399,
     4   21.650,   21.650,  21.650,  21.650,  21.650,  21.650,  21.650,
     5   21.897,   21.897,  21.897,  21.897,  21.897,  21.897,  21.897,
     6   22.139,   22.139,  22.139,  22.139,  22.139,  22.139,  22.139,
     7   22.378,   22.378,  22.378,  22.378,  22.378,  22.378,  22.378,
     8   22.614,   22.614,  22.614,  22.614,  22.614,  22.614,  22.614,
     9   22.848,   22.848,  22.848,  22.848,  22.848,  22.848,  22.848/
      data pf270/
     1   23.082,   23.082,  23.082,  23.082,  23.082,  23.082,  23.082,
     2   23.318,   23.318,  23.318,  23.318,  23.318,  23.318,  23.318,
     3   23.555,   23.555,  23.555,  23.555,  23.555,  23.555,  23.555,
     4   23.798,   23.798,  23.798,  23.798,  23.798,  23.798,  23.798,
     5   24.046,   24.046,  24.046,  24.046,  24.046,  24.046,  24.046,
     6   24.303,   24.303,  24.303,  24.303,  24.303,  24.303,  24.303,
     7   24.570,   24.570,  24.570,  24.570,  24.570,  24.570,  24.570,
     8   24.996,   24.996,  24.996,  24.996,  24.996,  24.996,  24.996,
     9   25.459,   25.459,  25.459,  25.459,  25.459,  25.459,  25.459/
      data pf271/
     1   25.968,   25.968,  25.968,  25.968,  25.968,  25.968,  25.968,
     2   26.530,   26.530,  26.530,  26.530,  26.530,  26.530,  26.530,
     3   27.156,   27.156,  27.156,  27.156,  27.156,  27.156,  27.156,
     4   27.851,   27.851,  27.851,  27.851,  27.851,  27.851,  27.851,
     5   28.625,   28.625,  28.625,  28.625,  28.625,  28.625,  28.625,
     6   29.483,   29.483,  29.483,  29.483,  29.483,  29.483,  29.483,
     7   30.430,   30.430,  30.430,  30.430,  30.430,  30.430,  30.430,
     8   31.472,   31.472,  31.472,  31.472,  31.472,  31.472,  31.472,
     9   33.425,   33.425,  33.425,  33.425,  33.425,  33.425,  33.425/
      data pf272/
     1   35.652,   35.652,  35.652,  35.652,  35.652,  35.652,  35.652,
     2   38.150,   38.150,  38.150,  38.150,  38.150,  38.150,  38.150,
     3   40.906,   40.906,  40.906,  40.906,  40.906,  40.906,  40.906,
     4   43.900,   43.900,  43.900,  43.900,  43.900,  43.900,  43.900,
     5   47.108,   47.108,  47.108,  47.108,  47.108,  47.108,  47.108,
     6   50.509,   50.509,  50.509,  50.509,  50.509,  50.509,  50.509,
     7   54.086,   54.086,  54.086,  54.086,  54.086,  54.086,  54.086,
     8   57.841,   57.841,  57.841,  57.841,  57.841,  57.841,  57.841,
     9   61.795,   61.795,  61.795,  61.795,  61.795,  61.795,  61.794/
      data pf273/
     1   66.012,   66.012,  66.012,  66.012,  66.012,  66.011,  66.009,
     2   70.622,   70.621,  70.621,  70.621,  70.619,  70.616,  70.606,
     3   75.873,   75.872,  75.872,  75.872,  75.863,  75.850,  75.803,
     4   82.248,   82.245,  82.243,  82.243,  82.209,  82.158,  81.985,
     5   90.661,   90.648,  90.643,  90.641,  90.526,  90.360,  89.804,
     6  102.787,  102.748, 102.732, 102.729, 102.387, 101.904, 100.337,
     7  121.508,  121.405, 121.360, 121.352, 120.453, 119.202, 115.254,
     8  151.404,  151.156, 151.049, 151.030, 148.899, 145.976, 136.985,
     9  199.164,  198.625, 198.392, 198.350, 193.751, 187.524, 168.799/
      data pf274/
     1  273.743,  272.663, 272.196, 272.112, 262.983, 250.767, 214.758,
     2  386.115,  384.109, 383.242, 383.087, 366.265, 343.992, 279.499,
     3  548.578,  545.097, 543.591, 543.324, 514.323, 476.280, 367.860,
     4  773.679,  767.986, 765.526, 765.089, 717.965, 656.664, 484.405,
     5 1072.906, 1064.084,1060.271,1059.596, 986.963, 893.175, 632.924,
     6 1455.419, 1442.382,1436.749,1435.753,1328.947,1191.941, 815.999,
     7 1926.998, 1908.533,1900.557,1899.149,1748.542,1556.482,1034.709,
     8   14.034,   14.034,  14.034,  14.034,  14.034,  14.034,  14.034,
     9   14.260,   14.260,  14.260,  14.260,  14.260,  14.260,  14.260/
      data pf275/
     1   14.483,   14.483,  14.483,  14.483,  14.483,  14.483,  14.483,
     2   14.701,   14.701,  14.701,  14.701,  14.701,  14.701,  14.701,
     3   14.916,   14.916,  14.916,  14.916,  14.916,  14.916,  14.916,
     4   15.126,   15.126,  15.126,  15.126,  15.126,  15.126,  15.126,
     5   15.332,   15.332,  15.332,  15.332,  15.332,  15.332,  15.332,
     6   15.535,   15.535,  15.535,  15.535,  15.535,  15.535,  15.535,
     7   15.733,   15.733,  15.733,  15.733,  15.733,  15.733,  15.733,
     8   15.928,   15.928,  15.928,  15.928,  15.928,  15.928,  15.928,
     9   16.119,   16.119,  16.119,  16.119,  16.119,  16.119,  16.119/
      data pf276/
     1   16.308,   16.308,  16.308,  16.308,  16.308,  16.308,  16.308,
     2   16.493,   16.493,  16.493,  16.493,  16.493,  16.493,  16.493,
     3   16.676,   16.676,  16.676,  16.676,  16.676,  16.676,  16.676,
     4   16.858,   16.858,  16.858,  16.858,  16.858,  16.858,  16.858,
     5   17.038,   17.038,  17.038,  17.038,  17.038,  17.038,  17.038,
     6   17.217,   17.217,  17.217,  17.217,  17.217,  17.217,  17.217,
     7   17.397,   17.397,  17.397,  17.397,  17.397,  17.397,  17.397,
     8   17.577,   17.577,  17.577,  17.577,  17.577,  17.577,  17.577,
     9   17.759,   17.759,  17.759,  17.759,  17.759,  17.759,  17.759/
      data pf277/
     1   18.037,   18.037,  18.037,  18.037,  18.037,  18.037,  18.037,
     2   18.322,   18.322,  18.322,  18.322,  18.322,  18.322,  18.322,
     3   18.618,   18.618,  18.618,  18.618,  18.618,  18.618,  18.618,
     4   18.928,   18.928,  18.928,  18.928,  18.928,  18.928,  18.928,
     5   19.253,   19.253,  19.253,  19.253,  19.253,  19.253,  19.253,
     6   19.597,   19.597,  19.597,  19.597,  19.597,  19.597,  19.597,
     7   19.962,   19.962,  19.962,  19.962,  19.962,  19.962,  19.962,
     8   20.348,   20.348,  20.348,  20.348,  20.348,  20.348,  20.348,
     9   20.758,   20.758,  20.758,  20.758,  20.758,  20.758,  20.758/
      data pf278/
     1   21.192,   21.192,  21.192,  21.192,  21.192,  21.192,  21.192,
     2   21.968,   21.968,  21.968,  21.968,  21.968,  21.968,  21.968,
     3   22.811,   22.811,  22.811,  22.811,  22.811,  22.811,  22.811,
     4   23.714,   23.714,  23.714,  23.714,  23.714,  23.714,  23.714,
     5   24.671,   24.671,  24.671,  24.671,  24.671,  24.671,  24.671,
     6   25.670,   25.670,  25.670,  25.670,  25.670,  25.670,  25.670,
     7   26.702,   26.702,  26.702,  26.702,  26.702,  26.702,  26.702,
     8   27.753,   27.753,  27.753,  27.753,  27.753,  27.753,  27.753,
     9   28.812,   28.812,  28.812,  28.812,  28.812,  28.812,  28.812/
      data pf279/
     1   29.869,   29.869,  29.869,  29.869,  29.869,  29.869,  29.869,
     2   30.916,   30.916,  30.916,  30.916,  30.916,  30.916,  30.916,
     3   31.948,   31.948,  31.948,  31.948,  31.948,  31.948,  31.948,
     4   32.967,   32.967,  32.967,  32.967,  32.967,  32.967,  32.967,
     5   33.984,   33.984,  33.984,  33.984,  33.984,  33.984,  33.984,
     6   35.025,   35.025,  35.025,  35.025,  35.025,  35.025,  35.024,
     7   36.147,   36.147,  36.147,  36.147,  36.147,  36.147,  36.145,
     8   37.462,   37.462,  37.462,  37.462,  37.462,  37.462,  37.455,
     9   39.188,   39.188,  39.188,  39.187,  39.187,  39.186,  39.160/
      data pf280/
     1   41.725,   41.725,  41.725,  41.723,  41.723,  41.720,  41.633,
     2   45.780,   45.780,  45.779,  45.775,  45.775,  45.764,  45.517,
     3   52.511,   52.511,  52.508,  52.497,  52.497,  52.468,  51.839,
     4   63.676,   63.673,  63.666,  63.641,  63.641,  63.572,  62.125,
     5   81.733,   81.728,  81.713,  81.658,  81.658,  81.510,  78.470,
     6  109.854,  109.843, 109.813, 109.703, 109.703, 109.412, 103.519,
     7  151.788,  151.769, 151.713, 151.509, 151.509, 150.974, 140.345,
     8  211.601,  211.569, 211.471, 211.118, 211.118, 210.200, 192.220,
     9  293.300,  293.247, 293.087, 292.511, 292.511, 291.025, 262.301/
      data pf281/
     1    7.141,    7.141,   7.141,   7.141,   7.141,   7.141,   7.141,
     2    7.234,    7.234,   7.234,   7.234,   7.234,   7.234,   7.234,
     3    7.325,    7.325,   7.325,   7.325,   7.325,   7.325,   7.325,
     4    7.414,    7.414,   7.414,   7.414,   7.414,   7.414,   7.414,
     5    7.502,    7.502,   7.502,   7.502,   7.502,   7.502,   7.502,
     6    7.588,    7.588,   7.588,   7.588,   7.588,   7.588,   7.588,
     7    7.672,    7.672,   7.672,   7.672,   7.672,   7.672,   7.672,
     8    7.754,    7.754,   7.754,   7.754,   7.754,   7.754,   7.754,
     9    7.834,    7.834,   7.834,   7.834,   7.834,   7.834,   7.834/
      data pf282/
     1    7.912,    7.912,   7.912,   7.912,   7.912,   7.912,   7.912,
     2    7.988,    7.988,   7.988,   7.988,   7.988,   7.988,   7.988,
     3    8.062,    8.062,   8.062,   8.062,   8.062,   8.062,   8.062,
     4    8.134,    8.134,   8.134,   8.134,   8.134,   8.134,   8.134,
     5    8.204,    8.204,   8.204,   8.204,   8.204,   8.204,   8.204,
     6    8.272,    8.272,   8.272,   8.272,   8.272,   8.272,   8.272,
     7    8.338,    8.338,   8.338,   8.338,   8.338,   8.338,   8.338,
     8    8.401,    8.401,   8.401,   8.401,   8.401,   8.401,   8.401,
     9    8.463,    8.463,   8.463,   8.463,   8.463,   8.463,   8.463/
      data pf283/
     1    8.523,    8.523,   8.523,   8.523,   8.523,   8.523,   8.523,
     2    8.581,    8.581,   8.581,   8.581,   8.581,   8.581,   8.581,
     3    8.664,    8.664,   8.664,   8.664,   8.664,   8.664,   8.664,
     4    8.743,    8.743,   8.743,   8.743,   8.743,   8.743,   8.743,
     5    8.818,    8.818,   8.818,   8.818,   8.818,   8.818,   8.818,
     6    8.889,    8.889,   8.889,   8.889,   8.889,   8.889,   8.889,
     7    8.957,    8.957,   8.957,   8.957,   8.957,   8.957,   8.957,
     8    9.020,    9.020,   9.020,   9.020,   9.020,   9.020,   9.020,
     9    9.080,    9.080,   9.080,   9.080,   9.080,   9.080,   9.080/
      data pf284/
     1    9.137,    9.137,   9.137,   9.137,   9.137,   9.137,   9.137,
     2    9.191,    9.191,   9.191,   9.191,   9.191,   9.191,   9.191,
     3    9.241,    9.241,   9.241,   9.241,   9.241,   9.241,   9.241,
     4    9.319,    9.319,   9.319,   9.319,   9.319,   9.319,   9.319,
     5    9.389,    9.389,   9.389,   9.389,   9.389,   9.389,   9.389,
     6    9.452,    9.452,   9.452,   9.452,   9.452,   9.452,   9.452,
     7    9.509,    9.509,   9.509,   9.509,   9.509,   9.509,   9.509,
     8    9.561,    9.561,   9.561,   9.561,   9.561,   9.561,   9.561,
     9    9.607,    9.607,   9.607,   9.607,   9.607,   9.607,   9.607/
      data pf285/
     1    9.648,    9.648,   9.648,   9.648,   9.648,   9.648,   9.648,
     2    9.685,    9.685,   9.685,   9.685,   9.685,   9.685,   9.685,
     3    9.719,    9.719,   9.719,   9.719,   9.719,   9.719,   9.719,
     4    9.749,    9.749,   9.749,   9.749,   9.749,   9.749,   9.749,
     5    9.776,    9.776,   9.776,   9.776,   9.776,   9.776,   9.776,
     6    9.801,    9.801,   9.801,   9.801,   9.801,   9.801,   9.801,
     7    9.825,    9.825,   9.825,   9.825,   9.825,   9.825,   9.825,
     8    9.853,    9.853,   9.853,   9.853,   9.853,   9.853,   9.853,
     9    9.891,    9.891,   9.891,   9.891,   9.891,   9.891,   9.891/
      data pf286/
     1    9.959,    9.959,   9.959,   9.959,   9.959,   9.959,   9.959,
     2   10.087,   10.087,  10.087,  10.087,  10.087,  10.087,  10.087,
     3   10.332,   10.332,  10.332,  10.332,  10.332,  10.332,  10.332,
     4   10.786,   10.786,  10.786,  10.786,  10.786,  10.786,  10.786,
     5   11.584,   11.584,  11.584,  11.584,  11.584,  11.584,  11.584,
     6   12.927,   12.927,  12.927,  12.927,  12.927,  12.927,  12.925,
     7   15.089,   15.089,  15.089,  15.089,  15.089,  15.087,  15.084,
     8   18.433,   18.433,  18.433,  18.433,  18.433,  18.429,  18.421,
     9   23.418,   23.418,  23.418,  23.418,  23.418,  23.409,  23.393/
      data pf287/
     1   30.592,   30.592,  30.592,  30.592,  30.592,  30.574,  30.540,
     2   40.566,   40.566,  40.566,  40.566,  40.566,  40.532,  40.470,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf288/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf289/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf290/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf291/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf292/
     1    1.002,    1.002,   1.002,   1.002,   1.002,   1.002,   1.002,
     2    1.005,    1.005,   1.005,   1.005,   1.005,   1.005,   1.005,
     3    1.013,    1.013,   1.013,   1.013,   1.013,   1.013,   1.013,
     4    1.033,    1.033,   1.033,   1.033,   1.033,   1.033,   1.033,
     5    1.076,    1.076,   1.076,   1.076,   1.076,   1.076,   1.076,
     6    1.158,    1.158,   1.158,   1.158,   1.158,   1.158,   1.158,
     7    1.310,    1.310,   1.310,   1.310,   1.310,   1.310,   1.310,
     8    1.576,    1.576,   1.576,   1.576,   1.576,   1.576,   1.576,
     9    2.031,    2.031,   2.031,   2.031,   2.031,   2.031,   2.031/
      data pf293/
     1    2.796,    2.796,   2.796,   2.796,   2.796,   2.796,   2.796,
     2    4.073,    4.073,   4.073,   4.073,   4.073,   4.073,   4.073,
     3    6.191,    6.191,   6.191,   6.191,   6.191,   6.191,   6.191,
     4    9.673,    9.673,   9.673,   9.673,   9.673,   9.673,   9.672,
     5    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     6    4.003,    4.003,   4.003,   4.003,   4.003,   4.003,   4.003,
     7    4.004,    4.004,   4.004,   4.004,   4.004,   4.004,   4.004,
     8    4.005,    4.005,   4.005,   4.005,   4.005,   4.005,   4.005,
     9    4.007,    4.007,   4.007,   4.007,   4.007,   4.007,   4.007/
      data pf294/
     1    4.009,    4.009,   4.009,   4.009,   4.009,   4.009,   4.009,
     2    4.011,    4.011,   4.011,   4.011,   4.011,   4.011,   4.011,
     3    4.014,    4.014,   4.014,   4.014,   4.014,   4.014,   4.014,
     4    4.018,    4.018,   4.018,   4.018,   4.018,   4.018,   4.018,
     5    4.022,    4.022,   4.022,   4.022,   4.022,   4.022,   4.022,
     6    4.027,    4.027,   4.027,   4.027,   4.027,   4.027,   4.027,
     7    4.033,    4.033,   4.033,   4.033,   4.033,   4.033,   4.033,
     8    4.040,    4.040,   4.040,   4.040,   4.040,   4.040,   4.040,
     9    4.047,    4.047,   4.047,   4.047,   4.047,   4.047,   4.047/
      data pf295/
     1    4.056,    4.056,   4.056,   4.056,   4.056,   4.056,   4.056,
     2    4.066,    4.066,   4.066,   4.066,   4.066,   4.066,   4.066,
     3    4.077,    4.077,   4.077,   4.077,   4.077,   4.077,   4.077,
     4    4.089,    4.089,   4.089,   4.089,   4.089,   4.089,   4.089,
     5    4.102,    4.102,   4.102,   4.102,   4.102,   4.102,   4.102,
     6    4.117,    4.117,   4.117,   4.117,   4.117,   4.117,   4.117,
     7    4.141,    4.141,   4.141,   4.141,   4.141,   4.141,   4.141,
     8    4.169,    4.169,   4.169,   4.169,   4.169,   4.169,   4.169,
     9    4.199,    4.199,   4.199,   4.199,   4.199,   4.199,   4.199/
      data pf296/
     1    4.232,    4.232,   4.232,   4.232,   4.232,   4.232,   4.232,
     2    4.268,    4.268,   4.268,   4.268,   4.268,   4.268,   4.268,
     3    4.306,    4.306,   4.306,   4.306,   4.306,   4.306,   4.306,
     4    4.347,    4.347,   4.347,   4.347,   4.347,   4.347,   4.347,
     5    4.390,    4.390,   4.390,   4.390,   4.390,   4.390,   4.390,
     6    4.435,    4.435,   4.435,   4.435,   4.435,   4.435,   4.435,
     7    4.482,    4.482,   4.482,   4.482,   4.482,   4.482,   4.482,
     8    4.563,    4.563,   4.563,   4.563,   4.563,   4.563,   4.563,
     9    4.646,    4.646,   4.646,   4.646,   4.646,   4.646,   4.646/
      data pf297/
     1    4.730,    4.730,   4.730,   4.730,   4.730,   4.730,   4.730,
     2    4.815,    4.815,   4.815,   4.815,   4.815,   4.815,   4.815,
     3    4.898,    4.898,   4.898,   4.898,   4.898,   4.898,   4.898,
     4    4.980,    4.980,   4.980,   4.980,   4.980,   4.980,   4.980,
     5    5.059,    5.059,   5.059,   5.059,   5.059,   5.059,   5.059,
     6    5.135,    5.135,   5.135,   5.135,   5.135,   5.135,   5.135,
     7    5.207,    5.207,   5.207,   5.207,   5.207,   5.207,   5.207,
     8    5.275,    5.275,   5.275,   5.275,   5.275,   5.275,   5.275,
     9    5.339,    5.339,   5.339,   5.339,   5.339,   5.339,   5.339/
      data pf298/
     1    5.399,    5.399,   5.399,   5.399,   5.399,   5.399,   5.399,
     2    5.456,    5.456,   5.456,   5.456,   5.456,   5.456,   5.456,
     3    5.511,    5.511,   5.511,   5.511,   5.511,   5.511,   5.511,
     4    5.567,    5.567,   5.567,   5.567,   5.567,   5.567,   5.567,
     5    5.633,    5.633,   5.633,   5.633,   5.633,   5.633,   5.633,
     6    5.723,    5.723,   5.723,   5.723,   5.723,   5.723,   5.723,
     7    5.862,    5.862,   5.862,   5.862,   5.862,   5.862,   5.862,
     8    6.092,    6.092,   6.092,   6.092,   6.092,   6.092,   6.092,
     9    6.476,    6.476,   6.476,   6.476,   6.476,   6.476,   6.476/
      data pf299/
     1    7.110,    7.110,   7.110,   7.110,   7.110,   7.110,   7.110,
     2    8.138,    8.138,   8.138,   8.138,   8.138,   8.138,   8.138,
     3    9.780,    9.780,   9.780,   9.780,   9.780,   9.780,   9.780,
     4   12.381,   12.381,  12.381,  12.381,  12.381,  12.381,  12.380,
     5   16.487,   16.487,  16.487,  16.487,  16.486,  16.486,  16.483,
     6   22.965,   22.965,  22.965,  22.965,  22.963,  22.961,  22.953,
     7    5.015,    5.015,   5.015,   5.015,   5.015,   5.015,   5.015,
     8    5.019,    5.019,   5.019,   5.019,   5.019,   5.019,   5.019,
     9    5.025,    5.025,   5.025,   5.025,   5.025,   5.025,   5.025/
      data pf300/
     1    5.031,    5.031,   5.031,   5.031,   5.031,   5.031,   5.031,
     2    5.038,    5.038,   5.038,   5.038,   5.038,   5.038,   5.038,
     3    5.047,    5.047,   5.047,   5.047,   5.047,   5.047,   5.047,
     4    5.057,    5.057,   5.057,   5.057,   5.057,   5.057,   5.057,
     5    5.069,    5.069,   5.069,   5.069,   5.069,   5.069,   5.069,
     6    5.083,    5.083,   5.083,   5.083,   5.083,   5.083,   5.083,
     7    5.098,    5.098,   5.098,   5.098,   5.098,   5.098,   5.098,
     8    5.116,    5.116,   5.116,   5.116,   5.116,   5.116,   5.116,
     9    5.136,    5.136,   5.136,   5.136,   5.136,   5.136,   5.136/
      data pf301/
     1    5.158,    5.158,   5.158,   5.158,   5.158,   5.158,   5.158,
     2    5.182,    5.182,   5.182,   5.182,   5.182,   5.182,   5.182,
     3    5.209,    5.209,   5.209,   5.209,   5.209,   5.209,   5.209,
     4    5.239,    5.239,   5.239,   5.239,   5.239,   5.239,   5.239,
     5    5.271,    5.271,   5.271,   5.271,   5.271,   5.271,   5.271,
     6    5.306,    5.306,   5.306,   5.306,   5.306,   5.306,   5.306,
     7    5.343,    5.343,   5.343,   5.343,   5.343,   5.343,   5.343,
     8    5.383,    5.383,   5.383,   5.383,   5.383,   5.383,   5.383,
     9    5.448,    5.448,   5.448,   5.448,   5.448,   5.448,   5.448/
      data pf302/
     1    5.519,    5.519,   5.519,   5.519,   5.519,   5.519,   5.519,
     2    5.596,    5.596,   5.596,   5.596,   5.596,   5.596,   5.596,
     3    5.678,    5.678,   5.678,   5.678,   5.678,   5.678,   5.678,
     4    5.766,    5.766,   5.766,   5.766,   5.766,   5.766,   5.766,
     5    5.860,    5.860,   5.860,   5.860,   5.860,   5.860,   5.860,
     6    5.959,    5.959,   5.959,   5.959,   5.959,   5.959,   5.959,
     7    6.064,    6.064,   6.064,   6.064,   6.064,   6.064,   6.064,
     8    6.175,    6.175,   6.175,   6.175,   6.175,   6.175,   6.175,
     9    6.291,    6.291,   6.291,   6.291,   6.291,   6.291,   6.291/
      data pf303/
     1    6.497,    6.497,   6.497,   6.497,   6.497,   6.497,   6.497,
     2    6.720,    6.720,   6.720,   6.720,   6.720,   6.720,   6.720,
     3    6.960,    6.960,   6.960,   6.960,   6.960,   6.960,   6.960,
     4    7.217,    7.217,   7.217,   7.217,   7.217,   7.217,   7.217,
     5    7.491,    7.491,   7.491,   7.491,   7.491,   7.491,   7.491,
     6    7.780,    7.780,   7.780,   7.780,   7.780,   7.780,   7.780,
     7    8.084,    8.084,   8.084,   8.084,   8.084,   8.084,   8.084,
     8    8.400,    8.400,   8.400,   8.400,   8.400,   8.400,   8.400,
     9    8.725,    8.725,   8.725,   8.725,   8.725,   8.725,   8.725/
      data pf304/
     1    9.058,    9.058,   9.058,   9.058,   9.058,   9.058,   9.058,
     2    9.395,    9.395,   9.395,   9.395,   9.395,   9.395,   9.395,
     3    9.734,    9.734,   9.734,   9.734,   9.734,   9.734,   9.734,
     4   10.074,   10.074,  10.074,  10.074,  10.074,  10.074,  10.074,
     5   10.415,   10.415,  10.415,  10.415,  10.415,  10.415,  10.415,
     6   10.761,   10.761,  10.761,  10.761,  10.761,  10.761,  10.761,
     7   11.124,   11.124,  11.124,  11.124,  11.124,  11.124,  11.124,
     8   11.526,   11.526,  11.526,  11.526,  11.526,  11.526,  11.526,
     9   12.004,   12.004,  12.004,  12.004,  12.004,  12.004,  12.004/
      data pf305/
     1   12.617,   12.617,  12.617,  12.617,  12.617,  12.617,  12.617,
     2   13.455,   13.455,  13.455,  13.455,  13.455,  13.455,  13.455,
     3   14.646,   14.646,  14.646,  14.646,  14.646,  14.646,  14.646,
     4   16.373,   16.373,  16.373,  16.373,  16.373,  16.373,  16.373,
     5   18.899,   18.899,  18.899,  18.899,  18.899,  18.899,  18.899,
     6   22.607,   22.607,  22.607,  22.607,  22.607,  22.607,  22.607,
     7   28.067,   28.067,  28.067,  28.067,  28.067,  28.067,  28.067,
     8   36.143,   36.143,  36.143,  36.143,  36.143,  36.143,  36.142,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf306/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf307/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf308/
     1    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     2    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     3    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     4    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     5    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     6    4.004,    4.004,   4.004,   4.004,   4.004,   4.004,   4.004,
     7    4.007,    4.007,   4.007,   4.007,   4.007,   4.007,   4.007,
     8    4.011,    4.011,   4.011,   4.011,   4.011,   4.011,   4.011,
     9    4.018,    4.018,   4.018,   4.018,   4.018,   4.018,   4.018/
      data pf309/
     1    4.027,    4.027,   4.027,   4.027,   4.027,   4.027,   4.027,
     2    4.040,    4.040,   4.040,   4.040,   4.040,   4.040,   4.040,
     3    4.074,    4.074,   4.074,   4.074,   4.074,   4.074,   4.074,
     4    4.127,    4.127,   4.127,   4.127,   4.127,   4.127,   4.127,
     5    4.207,    4.207,   4.207,   4.207,   4.207,   4.207,   4.207,
     6    4.320,    4.320,   4.320,   4.320,   4.320,   4.320,   4.320,
     7    4.474,    4.474,   4.474,   4.474,   4.474,   4.474,   4.474,
     8    4.676,    4.676,   4.676,   4.676,   4.676,   4.676,   4.676,
     9    4.930,    4.930,   4.930,   4.930,   4.930,   4.930,   4.930/
      data pf310/
     1    5.241,    5.241,   5.241,   5.241,   5.241,   5.241,   5.241,
     2    5.609,    5.609,   5.609,   5.609,   5.609,   5.609,   5.609,
     3    6.034,    6.034,   6.034,   6.034,   6.034,   6.034,   6.034,
     4    6.513,    6.513,   6.513,   6.513,   6.513,   6.513,   6.513,
     5    7.043,    7.043,   7.043,   7.043,   7.043,   7.043,   7.043,
     6    7.617,    7.617,   7.617,   7.617,   7.617,   7.617,   7.617,
     7    8.230,    8.230,   8.230,   8.230,   8.230,   8.230,   8.230,
     8    8.881,    8.881,   8.881,   8.881,   8.881,   8.881,   8.881,
     9    9.571,    9.571,   9.571,   9.571,   9.571,   9.571,   9.571/
      data pf311/
     1   10.308,   10.308,  10.308,  10.308,  10.308,  10.308,  10.308,
     2   11.111,   11.111,  11.111,  11.111,  11.111,  11.111,  11.111,
     3   12.018,   12.018,  12.018,  12.018,  12.018,  12.018,  12.018,
     4   13.085,   13.085,  13.085,  13.085,  13.085,  13.085,  13.085,
     5   14.400,   14.400,  14.400,  14.400,  14.400,  14.400,  14.400,
     6   16.090,   16.090,  16.090,  16.090,  16.090,  16.090,  16.090,
     7   18.338,   18.338,  18.338,  18.338,  18.338,  18.338,  18.338,
     8   21.407,   21.407,  21.407,  21.407,  21.407,  21.407,  21.407,
     9   25.671,   25.671,  25.671,  25.671,  25.671,  25.671,  25.671/
      data pf312/
     1   31.675,   31.675,  31.675,  31.675,  31.675,  31.675,  31.675,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     5    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     6    6.002,    6.002,   6.002,   6.002,   6.002,   6.002,   6.002,
     7    6.003,    6.003,   6.003,   6.003,   6.003,   6.003,   6.003,
     8    6.005,    6.005,   6.005,   6.005,   6.005,   6.005,   6.005,
     9    6.008,    6.008,   6.008,   6.008,   6.008,   6.008,   6.008/
      data pf313/
     1    6.012,    6.012,   6.012,   6.012,   6.012,   6.012,   6.012,
     2    6.018,    6.018,   6.018,   6.018,   6.018,   6.018,   6.018,
     3    6.026,    6.026,   6.026,   6.026,   6.026,   6.026,   6.026,
     4    6.037,    6.037,   6.037,   6.037,   6.037,   6.037,   6.037,
     5    6.052,    6.052,   6.052,   6.052,   6.052,   6.052,   6.052,
     6    6.072,    6.072,   6.072,   6.072,   6.072,   6.072,   6.072,
     7    6.099,    6.099,   6.099,   6.099,   6.099,   6.099,   6.098,
     8    6.134,    6.134,   6.134,   6.134,   6.134,   6.134,   6.133,
     9    6.180,    6.180,   6.180,   6.180,   6.180,   6.180,   6.177/
      data pf314/
     1    6.239,    6.239,   6.239,   6.239,   6.239,   6.239,   6.234,
     2    6.314,    6.314,   6.314,   6.314,   6.314,   6.313,   6.305,
     3    6.408,    6.408,   6.408,   6.408,   6.408,   6.407,   6.394,
     4    6.596,    6.596,   6.595,   6.595,   6.595,   6.593,   6.567,
     5    6.854,    6.853,   6.853,   6.853,   6.852,   6.847,   6.799,
     6    7.204,    7.204,   7.204,   7.203,   7.200,   7.189,   7.103,
     7    7.675,    7.675,   7.674,   7.672,   7.666,   7.643,   7.494,
     8    8.302,    8.302,   8.300,   8.294,   8.282,   8.237,   7.987,
     9    9.132,    9.130,   9.127,   9.114,   9.090,   9.004,   8.598/
      data pf315/
     1   10.223,   10.220,  10.212,  10.187,  10.138,   9.983,   9.343,
     2   11.651,   11.645,  11.630,  11.579,  11.488,  11.217,  10.237,
     3   13.511,   13.499,  13.471,  13.376,  13.210,  12.754,  11.293,
     4   15.923,   15.901,  15.850,  15.679,  15.390,  14.649,  12.523,
     5   21.582,   21.527,  21.398,  20.980,  20.308,  18.759,  14.986,
     6   30.063,   29.936,  29.644,  28.717,  27.292,  24.296,  17.986,
     7   42.469,   42.205,  41.600,  39.717,  36.928,  31.533,  21.527,
     8   60.113,   59.603,  58.448,  54.905,  49.829,  40.711,  25.589,
     9   84.432,   83.517,  81.458,  75.232,  66.578,  52.018,  30.131/
      data pf316/
     1  116.866,  115.326, 111.881, 101.592,  87.667,  65.561,  35.090,
     2  158.718,  156.268, 150.819, 134.718, 113.438,  81.357,  40.394,
     3  211.006,  207.301, 199.099, 175.103, 144.047,  99.328,  45.958,
     4  274.350,  268.992, 257.184, 222.938, 179.438, 119.302,  51.698,
     5  348.890,  341.448, 325.109, 278.089, 219.348, 141.031,  57.527,
     6  434.272,  424.298, 402.474, 340.101, 263.326, 164.209,  63.366,
     7  529.671,  516.721, 488.474, 408.240, 310.770, 188.489,  69.143,
     8  633.863,  617.521, 581.972, 481.550, 360.973, 213.511,  74.796,
     9  745.325,  725.217, 681.583, 558.924, 413.171, 238.916,  80.273/
      data pf317/
     1  862.342,  838.153, 785.776, 639.177, 466.585, 264.365,  85.531,
     2  983.119,  954.599, 892.963, 721.118, 520.464, 289.550,  90.540,
     3 1105.879, 1072.850,1001.590, 803.602, 574.112, 314.201,  95.277,
     4 1228.942, 1191.296,1110.200, 885.577, 626.909, 338.093,  99.728,
     5 1350.785, 1308.483,1217.483, 966.114, 678.328, 361.045, 103.888,
     6 1470.083, 1423.148,1322.304,1044.427, 727.935, 382.920, 107.755,
     7 1585.725, 1534.235,1423.725,1119.873, 775.393, 403.620, 111.334,
     8 1696.824, 1640.903,1520.998,1191.959, 820.451, 423.082, 114.632,
     9 1802.704, 1742.513,1613.566,1260.322, 862.944, 441.278, 117.662/
      data pf318/
     1 1902.888, 1838.618,1701.037,1324.725, 902.774, 458.201, 120.434,
     2 1997.075, 1928.938,1783.177,1385.037, 939.908, 473.870, 122.965,
     3 2085.118, 2013.338,1859.876,1441.219, 974.360, 488.318, 125.269,
     4    7.008,    7.008,   7.008,   7.008,   7.008,   7.008,   7.008,
     5    7.012,    7.012,   7.012,   7.012,   7.012,   7.012,   7.012,
     6    7.016,    7.016,   7.016,   7.016,   7.016,   7.016,   7.016,
     7    7.021,    7.021,   7.021,   7.021,   7.021,   7.021,   7.021,
     8    7.028,    7.028,   7.028,   7.028,   7.028,   7.028,   7.028,
     9    7.037,    7.037,   7.037,   7.037,   7.037,   7.037,   7.037/
      data pf319/
     1    7.048,    7.048,   7.048,   7.048,   7.048,   7.048,   7.048,
     2    7.062,    7.062,   7.062,   7.062,   7.062,   7.062,   7.062,
     3    7.079,    7.079,   7.079,   7.079,   7.079,   7.079,   7.079,
     4    7.100,    7.100,   7.100,   7.100,   7.100,   7.100,   7.100,
     5    7.126,    7.126,   7.126,   7.126,   7.126,   7.126,   7.126,
     6    7.158,    7.158,   7.158,   7.158,   7.158,   7.158,   7.158,
     7    7.196,    7.196,   7.196,   7.196,   7.196,   7.196,   7.196,
     8    7.242,    7.242,   7.242,   7.242,   7.242,   7.242,   7.242,
     9    7.296,    7.296,   7.296,   7.296,   7.296,   7.296,   7.296/
      data pf320/
     1    7.360,    7.360,   7.360,   7.360,   7.360,   7.360,   7.360,
     2    7.435,    7.435,   7.435,   7.435,   7.435,   7.435,   7.435,
     3    7.523,    7.523,   7.523,   7.523,   7.523,   7.523,   7.523,
     4    7.626,    7.626,   7.626,   7.626,   7.626,   7.626,   7.626,
     5    7.745,    7.745,   7.745,   7.745,   7.745,   7.745,   7.745,
     6    7.961,    7.961,   7.961,   7.961,   7.961,   7.961,   7.961,
     7    8.230,    8.230,   8.230,   8.230,   8.230,   8.230,   8.230,
     8    8.564,    8.564,   8.564,   8.564,   8.564,   8.564,   8.564,
     9    8.977,    8.977,   8.977,   8.977,   8.977,   8.977,   8.977/
      data pf321/
     1    9.489,    9.489,   9.489,   9.489,   9.489,   9.489,   9.489,
     2   10.123,   10.123,  10.123,  10.123,  10.123,  10.123,  10.123,
     3   10.909,   10.909,  10.909,  10.909,  10.909,  10.909,  10.909,
     4   11.882,   11.882,  11.882,  11.882,  11.882,  11.882,  11.882,
     5   13.084,   13.084,  13.084,  13.084,  13.084,  13.083,  13.083,
     6   14.565,   14.565,  14.565,  14.565,  14.565,  14.565,  14.564,
     7   17.822,   17.822,  17.822,  17.822,  17.821,  17.820,  17.818,
     8   22.368,   22.368,  22.368,  22.367,  22.366,  22.361,  22.350,
     9   28.652,   28.651,  28.650,  28.648,  28.641,  28.622,  28.583/
      data pf322/
     1   37.272,   37.265,  37.261,  37.255,  37.229,  37.161,  37.039,
     2   49.043,   49.019,  49.006,  48.987,  48.900,  48.692,  48.349,
     3   65.099,   65.028,  64.990,  64.932,  64.683,  64.117,  63.257,
     4   87.015,   86.823,  86.723,  86.572,  85.936,  84.551,  82.599,
     5  116.918,  116.457, 116.218, 115.860, 114.392, 111.318, 107.262,
     6  157.560,  156.554, 156.033, 155.262, 152.170, 145.912, 138.119,
     7  212.291,  210.274, 209.232, 207.705, 201.699, 189.905, 175.953,
     8  284.914,  281.165, 279.231, 276.424, 265.568, 244.820, 221.361,
     9  379.407,  372.890, 369.538, 364.706, 346.308, 311.978, 274.690/
      data pf323/
     1  499.550,  488.886, 483.411, 475.572, 446.129, 392.351, 335.979,
     2  648.525,  631.984, 623.506, 611.439, 566.670, 486.433, 404.938,
     3  828.534,  804.072, 791.555, 773.830, 708.790, 594.169, 480.965,
     4 1040.529, 1005.861, 988.148, 963.179, 872.449, 714.932, 563.181,
     5 1284.073, 1236.770,1212.631,1178.745,1056.676, 847.559, 650.498,
     6 1557.351, 1494.951,1463.145,1418.658,1259.640, 990.438, 741.686,
     7 1857.310, 1777.438,1736.767,1680.068,1478.787,1141.620, 835.452,
     8 2179.909, 2080.380,2029.747,1959.362,1711.034,1298.948, 930.508,
     9 2520.412, 2399.322,2337.770,2252.428,1952.973,1460.186,1025.630/
      data pf324/
     1 2873.715, 2729.501,2656.246,2554.915,2201.084,1623.137,1119.703,
     2 3234.634, 3066.114,2980.568,2862.477,2451.914,1785.734,1211.747,
     3 3598.172, 3404.556,3306.327,3170.977,2702.226,1946.112,1300.940,
     4 3959.710, 3740.594,3629.485,3476.636,2949.111,2102.654,1386.619,
     5 4315.152, 4070.492,3946.486,3776.144,3190.051,2254.014,1468.278,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     8    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     9    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000/
      data pf325/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     5    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     8    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     9    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001/
      data pf326/
     1    6.002,    6.002,   6.002,   6.002,   6.002,   6.002,   6.002,
     2    6.003,    6.003,   6.003,   6.003,   6.003,   6.003,   6.003,
     3    6.004,    6.004,   6.004,   6.004,   6.004,   6.004,   6.004,
     4    6.007,    6.007,   6.007,   6.007,   6.007,   6.007,   6.007,
     5    6.010,    6.010,   6.010,   6.010,   6.010,   6.010,   6.010,
     6    6.015,    6.015,   6.015,   6.015,   6.015,   6.015,   6.015,
     7    6.021,    6.021,   6.021,   6.021,   6.021,   6.021,   6.021,
     8    6.037,    6.037,   6.037,   6.037,   6.037,   6.037,   6.037,
     9    6.061,    6.061,   6.061,   6.061,   6.061,   6.061,   6.061/
      data pf327/
     1    6.098,    6.098,   6.098,   6.098,   6.098,   6.098,   6.098,
     2    6.154,    6.154,   6.154,   6.154,   6.154,   6.154,   6.154,
     3    6.234,    6.234,   6.234,   6.234,   6.234,   6.234,   6.234,
     4    6.349,    6.349,   6.349,   6.349,   6.349,   6.349,   6.349,
     5    6.508,    6.508,   6.508,   6.508,   6.508,   6.508,   6.508,
     6    6.724,    6.724,   6.724,   6.724,   6.724,   6.724,   6.724,
     7    7.012,    7.012,   7.012,   7.012,   7.012,   7.012,   7.012,
     8    7.390,    7.390,   7.390,   7.390,   7.390,   7.390,   7.390,
     9    8.271,    8.271,   8.271,   8.271,   8.271,   8.271,   8.271/
      data pf328/
     1    9.558,    9.558,   9.558,   9.558,   9.558,   9.558,   9.558,
     2   11.370,   11.370,  11.370,  11.370,  11.370,  11.370,  11.370,
     3   13.842,   13.842,  13.842,  13.842,  13.842,  13.842,  13.842,
     4   17.128,   17.128,  17.128,  17.128,  17.128,  17.128,  17.128,
     5   21.406,   21.406,  21.406,  21.406,  21.406,  21.406,  21.406,
     6   26.899,   26.899,  26.899,  26.899,  26.899,  26.898,  26.897,
     7   33.902,   33.902,  33.902,  33.902,  33.901,  33.897,  33.890,
     8   42.848,   42.847,  42.846,  42.844,  42.838,  42.820,  42.789,
     9   54.423,   54.423,  54.417,  54.408,  54.380,  54.306,  54.184/
      data pf329/
     1   69.802,   69.798,  69.779,  69.743,  69.638,  69.372,  68.955,
     2   91.026,   91.014,  90.950,  90.833,  90.494,  89.665,  88.417,
     3  121.592,  121.557, 121.371, 121.034, 120.073, 117.785, 114.472,
     4  167.185,  167.095, 166.615, 165.752, 163.320, 157.659, 149.749,
     5  236.428,  236.219, 235.104, 233.108, 227.543, 214.855, 197.666,
     6  341.417,  340.971, 338.608, 334.394, 322.756, 296.704, 262.369,
     7  497.755,  496.883, 492.266, 484.060, 461.599, 412.136, 348.511,
     8  723.929,  722.343, 713.956, 699.095, 658.737, 571.146, 460.881,
     9 1039.969, 1037.265,1022.990, 997.759, 929.721, 783.952, 603.935/
      data pf330/
     1 1465.575, 1461.226,1438.293,1397.852,1289.481,1059.963, 781.305,
     2 2018.013, 2011.370,1976.380,1914.800,1750.707,1406.727, 995.368,
     3 2710.155, 2700.464,2649.475,2559.900,2322.394,1829.055,1246.958,
     4 3548.996, 3535.428,3464.108,3339.013,3008.798,2328.452,1535.239,
     5 4534.858, 4516.543,4420.358,4251.890,3808.940,2902.921,1857.764,
     6 5661.329, 5637.401,5511.833,5292.182,4716.685,3547.139,2210.678,
     7 6915.891, 6885.525,6726.283,6448.041,5721.321,4252.933,2589.032,
     8   17.582,   17.582,  17.582,  17.582,  17.582,  17.582,  17.582,
     9   17.846,   17.846,  17.846,  17.846,  17.846,  17.846,  17.846/
      data pf331/
     1   18.103,   18.103,  18.103,  18.103,  18.103,  18.103,  18.103,
     2   18.354,   18.354,  18.354,  18.354,  18.354,  18.354,  18.354,
     3   18.598,   18.598,  18.598,  18.598,  18.598,  18.598,  18.598,
     4   18.835,   18.835,  18.835,  18.835,  18.835,  18.835,  18.835,
     5   19.066,   19.066,  19.066,  19.066,  19.066,  19.066,  19.066,
     6   19.290,   19.290,  19.290,  19.290,  19.290,  19.290,  19.290,
     7   19.507,   19.507,  19.507,  19.507,  19.507,  19.507,  19.507,
     8   19.719,   19.719,  19.719,  19.719,  19.719,  19.719,  19.719,
     9   19.924,   19.924,  19.924,  19.924,  19.924,  19.924,  19.924/
      data pf332/
     1   20.124,   20.124,  20.124,  20.124,  20.124,  20.124,  20.124,
     2   20.319,   20.319,  20.319,  20.319,  20.319,  20.319,  20.319,
     3   20.510,   20.510,  20.510,  20.510,  20.510,  20.510,  20.510,
     4   20.696,   20.696,  20.696,  20.696,  20.696,  20.696,  20.696,
     5   20.880,   20.880,  20.880,  20.880,  20.880,  20.880,  20.880,
     6   21.061,   21.061,  21.061,  21.061,  21.061,  21.061,  21.061,
     7   21.243,   21.243,  21.243,  21.243,  21.243,  21.243,  21.243,
     8   21.425,   21.425,  21.425,  21.425,  21.425,  21.425,  21.425,
     9   21.611,   21.611,  21.611,  21.611,  21.611,  21.611,  21.611/
      data pf333/
     1   21.899,   21.899,  21.899,  21.899,  21.899,  21.899,  21.899,
     2   22.208,   22.208,  22.208,  22.208,  22.208,  22.208,  22.208,
     3   22.547,   22.547,  22.547,  22.547,  22.547,  22.547,  22.547,
     4   22.928,   22.928,  22.928,  22.928,  22.928,  22.928,  22.928,
     5   23.365,   23.365,  23.365,  23.365,  23.365,  23.365,  23.365,
     6   23.872,   23.872,  23.872,  23.872,  23.872,  23.872,  23.872,
     7   24.464,   24.464,  24.464,  24.464,  24.464,  24.464,  24.464,
     8   25.158,   25.158,  25.158,  25.158,  25.158,  25.158,  25.158,
     9   25.970,   25.970,  25.970,  25.970,  25.970,  25.970,  25.970/
      data pf334/
     1   26.915,   26.915,  26.915,  26.915,  26.915,  26.915,  26.915,
     2   28.831,   28.831,  28.831,  28.831,  28.831,  28.831,  28.831,
     3   31.225,   31.225,  31.225,  31.225,  31.225,  31.225,  31.225,
     4   34.150,   34.150,  34.150,  34.150,  34.150,  34.150,  34.150,
     5   37.640,   37.640,  37.640,  37.640,  37.640,  37.640,  37.640,
     6   41.714,   41.714,  41.714,  41.714,  41.714,  41.714,  41.714,
     7   46.375,   46.375,  46.375,  46.375,  46.375,  46.375,  46.375,
     8   51.614,   51.614,  51.614,  51.614,  51.614,  51.614,  51.614,
     9   57.419,   57.419,  57.419,  57.419,  57.419,  57.419,  57.419/
      data pf335/
     1   63.790,   63.790,  63.790,  63.790,  63.790,  63.790,  63.790,
     2   70.753,   70.753,  70.753,  70.753,  70.753,  70.753,  70.753,
     3   78.395,   78.395,  78.395,  78.395,  78.395,  78.394,  78.392,
     4   86.906,   86.906,  86.905,  86.904,  86.902,  86.899,  86.887,
     5   96.668,   96.667,  96.663,  96.656,  96.648,  96.632,  96.577,
     6  108.433,  108.427, 108.412, 108.384, 108.348, 108.285, 108.068,
     7  123.653,  123.631, 123.578, 123.477, 123.349, 123.127, 122.397,
     8  145.054,  144.982, 144.815, 144.500, 144.102, 143.424, 141.265,
     9  177.466,  177.267, 176.804, 175.933, 174.843, 173.004, 167.329/
      data pf336/
     1  228.856,  228.360, 227.211, 225.054, 222.378, 217.906, 204.477,
     2  311.317,  310.201, 307.616, 302.778, 296.818, 286.941, 258.004,
     3  441.695,  439.394, 434.071, 424.131, 411.963, 391.950, 334.585,
     4  641.489,  637.103, 626.970, 608.086, 585.102, 547.548, 441.979,
     5  935.823,  928.028, 910.043, 876.586, 836.073, 770.263, 588.445,
     6 1351.488, 1338.476,1308.487,1252.784,1185.640,1077.140, 781.968,
     7 1914.345, 1893.802,1846.501,1758.762,1653.430,1484.014,1029.405,
     8 2646.519, 2615.658,2544.657,2413.119,2255.778,2003.756,1335.700,
     9 3563.888, 3519.532,3417.562,3228.859,3003.863,2644.803,1703.317/
      data pf337/
     1   16.969,   16.969,  16.969,  16.969,  16.969,  16.969,  16.969,
     2   17.308,   17.308,  17.308,  17.308,  17.308,  17.308,  17.308,
     3   17.643,   17.643,  17.643,  17.643,  17.643,  17.643,  17.643,
     4   17.973,   17.973,  17.973,  17.973,  17.973,  17.973,  17.973,
     5   18.298,   18.298,  18.298,  18.298,  18.298,  18.298,  18.298,
     6   18.617,   18.617,  18.617,  18.617,  18.617,  18.617,  18.617,
     7   18.931,   18.931,  18.931,  18.931,  18.931,  18.931,  18.931,
     8   19.239,   19.239,  19.239,  19.239,  19.239,  19.239,  19.239,
     9   19.541,   19.541,  19.541,  19.541,  19.541,  19.541,  19.541/
      data pf338/
     1   19.838,   19.838,  19.838,  19.838,  19.838,  19.838,  19.838,
     2   20.129,   20.129,  20.129,  20.129,  20.129,  20.129,  20.129,
     3   20.415,   20.415,  20.415,  20.415,  20.415,  20.415,  20.415,
     4   20.696,   20.696,  20.696,  20.696,  20.696,  20.696,  20.696,
     5   20.973,   20.973,  20.973,  20.973,  20.973,  20.973,  20.973,
     6   21.247,   21.247,  21.247,  21.247,  21.247,  21.247,  21.247,
     7   21.518,   21.518,  21.518,  21.518,  21.518,  21.518,  21.518,
     8   21.788,   21.788,  21.788,  21.788,  21.788,  21.788,  21.788,
     9   22.058,   22.058,  22.058,  22.058,  22.058,  22.058,  22.058/
      data pf339/
     1   22.328,   22.328,  22.328,  22.328,  22.328,  22.328,  22.328,
     2   22.602,   22.602,  22.602,  22.602,  22.602,  22.602,  22.602,
     3   23.021,   23.021,  23.021,  23.021,  23.021,  23.021,  23.021,
     4   23.456,   23.456,  23.456,  23.456,  23.456,  23.456,  23.456,
     5   23.915,   23.915,  23.915,  23.915,  23.915,  23.915,  23.915,
     6   24.406,   24.406,  24.406,  24.406,  24.406,  24.406,  24.406,
     7   24.937,   24.937,  24.937,  24.937,  24.937,  24.937,  24.937,
     8   25.517,   25.517,  25.517,  25.517,  25.517,  25.517,  25.517,
     9   26.153,   26.153,  26.153,  26.153,  26.153,  26.153,  26.153/
      data pf340/
     1   26.854,   26.854,  26.854,  26.854,  26.854,  26.854,  26.854,
     2   27.629,   27.629,  27.629,  27.629,  27.629,  27.629,  27.629,
     3   28.482,   28.482,  28.482,  28.482,  28.482,  28.482,  28.482,
     4   30.099,   30.099,  30.099,  30.099,  30.099,  30.099,  30.099,
     5   31.973,   31.973,  31.973,  31.973,  31.973,  31.973,  31.973,
     6   34.117,   34.117,  34.117,  34.117,  34.117,  34.117,  34.117,
     7   36.530,   36.530,  36.530,  36.530,  36.530,  36.530,  36.530,
     8   39.204,   39.204,  39.204,  39.204,  39.204,  39.204,  39.204,
     9   42.121,   42.121,  42.121,  42.121,  42.121,  42.121,  42.121/
      data pf341/
     1   45.253,   45.253,  45.253,  45.253,  45.253,  45.253,  45.253,
     2   48.570,   48.570,  48.570,  48.570,  48.570,  48.570,  48.570,
     3   52.038,   52.038,  52.038,  52.038,  52.038,  52.038,  52.038,
     4   55.623,   55.623,  55.623,  55.623,  55.623,  55.623,  55.623,
     5   59.295,   59.295,  59.295,  59.295,  59.295,  59.295,  59.295,
     6   63.037,   63.037,  63.037,  63.037,  63.037,  63.037,  63.037,
     7   66.856,   66.856,  66.856,  66.856,  66.856,  66.856,  66.856,
     8   70.791,   70.791,  70.791,  70.791,  70.791,  70.791,  70.790,
     9   74.951,   74.951,  74.951,  74.951,  74.951,  74.950,  74.946/
      data pf342/
     1   79.561,   79.561,  79.561,  79.561,  79.559,  79.554,  79.538,
     2   85.072,   85.072,  85.072,  85.072,  85.063,  85.044,  84.978,
     3   92.353,   92.353,  92.353,  92.352,  92.322,  92.254,  92.028,
     4  103.009,  103.009, 103.009, 103.006, 102.910, 102.702, 102.025,
     5  119.821,  119.819, 119.819, 119.810, 119.548, 118.981, 117.179,
     6  147.257,  147.252, 147.252, 147.231, 146.584, 145.200, 140.888,
     7  191.936,  191.924, 191.924, 191.877, 190.430, 187.367, 177.980,
     8  262.857,  262.831, 262.831, 262.734, 259.770, 253.552, 234.773,
     9  371.251,  371.201, 371.201, 371.017, 365.399, 353.711, 318.872/
      data pf343/
     1  529.958,  529.868, 529.868, 529.542, 519.609, 499.097, 438.662,
     2  752.369,  752.220, 752.220, 751.675, 735.169, 701.308, 602.566,
     3   12.040,   12.040,  12.040,  12.040,  12.040,  12.040,  12.040,
     4   12.289,   12.289,  12.289,  12.289,  12.289,  12.289,  12.289,
     5   12.537,   12.537,  12.537,  12.537,  12.537,  12.537,  12.537,
     6   12.783,   12.783,  12.783,  12.783,  12.783,  12.783,  12.783,
     7   13.027,   13.027,  13.027,  13.027,  13.027,  13.027,  13.027,
     8   13.268,   13.268,  13.268,  13.268,  13.268,  13.268,  13.268,
     9   13.507,   13.507,  13.507,  13.507,  13.507,  13.507,  13.507/
      data pf344/
     1   13.742,   13.742,  13.742,  13.742,  13.742,  13.742,  13.742,
     2   13.975,   13.975,  13.975,  13.975,  13.975,  13.975,  13.975,
     3   14.204,   14.204,  14.204,  14.204,  14.204,  14.204,  14.204,
     4   14.430,   14.430,  14.430,  14.430,  14.430,  14.430,  14.430,
     5   14.652,   14.652,  14.652,  14.652,  14.652,  14.652,  14.652,
     6   14.872,   14.872,  14.872,  14.872,  14.872,  14.872,  14.872,
     7   15.088,   15.088,  15.088,  15.088,  15.088,  15.088,  15.088,
     8   15.302,   15.302,  15.302,  15.302,  15.302,  15.302,  15.302,
     9   15.513,   15.513,  15.513,  15.513,  15.513,  15.513,  15.513/
      data pf345/
     1   15.721,   15.721,  15.721,  15.721,  15.721,  15.721,  15.721,
     2   15.928,   15.928,  15.928,  15.928,  15.928,  15.928,  15.928,
     3   16.134,   16.134,  16.134,  16.134,  16.134,  16.134,  16.134,
     4   16.338,   16.338,  16.338,  16.338,  16.338,  16.338,  16.338,
     5   16.645,   16.645,  16.645,  16.645,  16.645,  16.645,  16.645,
     6   16.954,   16.954,  16.954,  16.954,  16.954,  16.954,  16.954,
     7   17.266,   17.266,  17.266,  17.266,  17.266,  17.266,  17.266,
     8   17.586,   17.586,  17.586,  17.586,  17.586,  17.586,  17.586,
     9   17.915,   17.915,  17.915,  17.915,  17.915,  17.915,  17.915/
      data pf346/
     1   18.256,   18.256,  18.256,  18.256,  18.256,  18.256,  18.256,
     2   18.612,   18.612,  18.612,  18.612,  18.612,  18.612,  18.612,
     3   18.985,   18.985,  18.985,  18.985,  18.985,  18.985,  18.985,
     4   19.377,   19.377,  19.377,  19.377,  19.377,  19.377,  19.377,
     5   19.790,   19.790,  19.790,  19.790,  19.790,  19.790,  19.790,
     6   20.526,   20.526,  20.526,  20.526,  20.526,  20.526,  20.526,
     7   21.327,   21.327,  21.327,  21.327,  21.327,  21.327,  21.327,
     8   22.191,   22.191,  22.191,  22.191,  22.191,  22.191,  22.191,
     9   23.115,   23.115,  23.115,  23.115,  23.115,  23.115,  23.115/
      data pf347/
     1   24.091,   24.091,  24.091,  24.091,  24.091,  24.091,  24.091,
     2   25.110,   25.110,  25.110,  25.110,  25.110,  25.110,  25.110,
     3   26.161,   26.161,  26.161,  26.161,  26.161,  26.161,  26.161,
     4   27.233,   27.233,  27.233,  27.233,  27.233,  27.233,  27.233,
     5   28.313,   28.313,  28.313,  28.313,  28.313,  28.313,  28.313,
     6   29.391,   29.391,  29.391,  29.391,  29.391,  29.391,  29.391,
     7   30.455,   30.455,  30.455,  30.455,  30.455,  30.455,  30.455,
     8   31.498,   31.498,  31.498,  31.498,  31.498,  31.498,  31.498,
     9   32.513,   32.513,  32.513,  32.513,  32.513,  32.513,  32.513/
      data pf348/
     1   33.499,   33.499,  33.499,  33.499,  33.499,  33.499,  33.499,
     2   34.465,   34.465,  34.465,  34.465,  34.465,  34.465,  34.465,
     3   35.436,   35.436,  35.436,  35.436,  35.436,  35.436,  35.436,
     4   36.466,   36.466,  36.466,  36.466,  36.466,  36.466,  36.466,
     5   37.667,   37.667,  37.667,  37.667,  37.667,  37.667,  37.666,
     6   39.247,   39.247,  39.246,  39.246,  39.246,  39.246,  39.244,
     7   41.574,   41.574,  41.574,  41.574,  41.573,  41.572,  41.565,
     8   45.281,   45.281,  45.281,  45.279,  45.276,  45.274,  45.250,
     9   51.395,   51.395,  51.394,  51.390,  51.382,  51.374,  51.307/
      data pf349/
     1   61.500,   61.500,  61.496,  61.486,  61.465,  61.444,  61.275,
     2   77.883,   77.883,  77.874,  77.849,  77.801,  77.753,  77.366,
     3  103.622,  103.622, 103.604, 103.550, 103.447, 103.345, 102.538,
     4  142.555,  142.555, 142.520, 142.414, 142.213, 142.014, 140.459,
     5    6.388,    6.388,   6.388,   6.388,   6.388,   6.388,   6.388,
     6    6.489,    6.489,   6.489,   6.489,   6.489,   6.489,   6.489,
     7    6.589,    6.589,   6.589,   6.589,   6.589,   6.589,   6.589,
     8    6.689,    6.689,   6.689,   6.689,   6.689,   6.689,   6.689,
     9    6.788,    6.788,   6.788,   6.788,   6.788,   6.788,   6.788/
      data pf350/
     1    6.886,    6.886,   6.886,   6.886,   6.886,   6.886,   6.886,
     2    6.983,    6.983,   6.983,   6.983,   6.983,   6.983,   6.983,
     3    7.078,    7.078,   7.078,   7.078,   7.078,   7.078,   7.078,
     4    7.172,    7.172,   7.172,   7.172,   7.172,   7.172,   7.172,
     5    7.264,    7.264,   7.264,   7.264,   7.264,   7.264,   7.264,
     6    7.355,    7.355,   7.355,   7.355,   7.355,   7.355,   7.355,
     7    7.444,    7.444,   7.444,   7.444,   7.444,   7.444,   7.444,
     8    7.531,    7.531,   7.531,   7.531,   7.531,   7.531,   7.531,
     9    7.616,    7.616,   7.616,   7.616,   7.616,   7.616,   7.616/
      data pf351/
     1    7.699,    7.699,   7.699,   7.699,   7.699,   7.699,   7.699,
     2    7.781,    7.781,   7.781,   7.781,   7.781,   7.781,   7.781,
     3    7.860,    7.860,   7.860,   7.860,   7.860,   7.860,   7.860,
     4    7.938,    7.938,   7.938,   7.938,   7.938,   7.938,   7.938,
     5    8.013,    8.013,   8.013,   8.013,   8.013,   8.013,   8.013,
     6    8.086,    8.086,   8.086,   8.086,   8.086,   8.086,   8.086,
     7    8.192,    8.192,   8.192,   8.192,   8.192,   8.192,   8.192,
     8    8.294,    8.294,   8.294,   8.294,   8.294,   8.294,   8.294,
     9    8.391,    8.391,   8.391,   8.391,   8.391,   8.391,   8.391/
      data pf352/
     1    8.483,    8.483,   8.483,   8.483,   8.483,   8.483,   8.483,
     2    8.571,    8.571,   8.571,   8.571,   8.571,   8.571,   8.571,
     3    8.655,    8.655,   8.655,   8.655,   8.655,   8.655,   8.655,
     4    8.735,    8.735,   8.735,   8.735,   8.735,   8.735,   8.735,
     5    8.810,    8.810,   8.810,   8.810,   8.810,   8.810,   8.810,
     6    8.882,    8.882,   8.882,   8.882,   8.882,   8.882,   8.882,
     7    8.949,    8.949,   8.949,   8.949,   8.949,   8.949,   8.949,
     8    9.054,    9.054,   9.054,   9.054,   9.054,   9.054,   9.054,
     9    9.149,    9.149,   9.149,   9.149,   9.149,   9.149,   9.149/
      data pf353/
     1    9.236,    9.236,   9.236,   9.236,   9.236,   9.236,   9.236,
     2    9.314,    9.314,   9.314,   9.314,   9.314,   9.314,   9.314,
     3    9.384,    9.384,   9.384,   9.384,   9.384,   9.384,   9.384,
     4    9.448,    9.448,   9.448,   9.448,   9.448,   9.448,   9.448,
     5    9.506,    9.506,   9.506,   9.506,   9.506,   9.506,   9.506,
     6    9.557,    9.557,   9.557,   9.557,   9.557,   9.557,   9.557,
     7    9.604,    9.604,   9.604,   9.604,   9.604,   9.604,   9.604,
     8    9.646,    9.646,   9.646,   9.646,   9.646,   9.646,   9.646,
     9    9.683,    9.683,   9.683,   9.683,   9.683,   9.683,   9.683/
      data pf354/
     1    9.717,    9.717,   9.717,   9.717,   9.717,   9.717,   9.717,
     2    9.748,    9.748,   9.748,   9.748,   9.748,   9.748,   9.748,
     3    9.777,    9.777,   9.777,   9.777,   9.777,   9.777,   9.777,
     4    9.809,    9.809,   9.809,   9.809,   9.809,   9.809,   9.809,
     5    9.851,    9.851,   9.851,   9.851,   9.851,   9.851,   9.851,
     6    9.921,    9.921,   9.921,   9.921,   9.921,   9.921,   9.921,
     7   10.051,   10.051,  10.051,  10.051,  10.051,  10.051,  10.051,
     8   10.296,   10.296,  10.296,  10.296,  10.296,  10.296,  10.296,
     9   10.740,   10.740,  10.740,  10.740,  10.740,  10.740,  10.739/
      data pf355/
     1   11.506,   11.506,  11.505,  11.505,  11.505,  11.505,  11.504,
     2   12.764,   12.764,  12.764,  12.764,  12.764,  12.762,  12.759,
     3   14.746,   14.746,  14.745,  14.745,  14.745,  14.738,  14.729,
     4   17.758,   17.758,  17.758,  17.758,  17.755,  17.738,  17.710,
     5   22.207,   22.207,  22.206,  22.206,  22.199,  22.154,  22.084,
     6   28.615,   28.615,  28.611,  28.611,  28.595,  28.492,  28.333,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf356/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf357/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf358/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf359/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf360/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     6    1.002,    1.002,   1.002,   1.002,   1.002,   1.002,   1.002,
     7    1.006,    1.006,   1.006,   1.006,   1.006,   1.006,   1.006,
     8    1.016,    1.016,   1.016,   1.016,   1.016,   1.016,   1.016,
     9    1.039,    1.039,   1.039,   1.039,   1.039,   1.039,   1.039/
      data pf361/
     1    1.088,    1.088,   1.088,   1.088,   1.088,   1.088,   1.088,
     2    1.181,    1.181,   1.181,   1.181,   1.181,   1.181,   1.181,
     3    1.349,    1.349,   1.349,   1.349,   1.349,   1.349,   1.349,
     4    1.642,    1.642,   1.642,   1.642,   1.642,   1.642,   1.642,
     5    2.134,    2.134,   2.134,   2.134,   2.134,   2.134,   2.134,
     6    2.948,    2.948,   2.948,   2.948,   2.948,   2.948,   2.948,
     7    4.273,    4.273,   4.273,   4.273,   4.273,   4.273,   4.273,
     8    6.412,    6.412,   6.412,   6.412,   6.412,   6.412,   6.412,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf362/
     1    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     2    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     3    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     4    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     5    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     6    4.003,    4.003,   4.003,   4.003,   4.003,   4.003,   4.003,
     7    4.004,    4.004,   4.004,   4.004,   4.004,   4.004,   4.004,
     8    4.005,    4.005,   4.005,   4.005,   4.005,   4.005,   4.005,
     9    4.007,    4.007,   4.007,   4.007,   4.007,   4.007,   4.007/
      data pf363/
     1    4.009,    4.009,   4.009,   4.009,   4.009,   4.009,   4.009,
     2    4.011,    4.011,   4.011,   4.011,   4.011,   4.011,   4.011,
     3    4.014,    4.014,   4.014,   4.014,   4.014,   4.014,   4.014,
     4    4.017,    4.017,   4.017,   4.017,   4.017,   4.017,   4.017,
     5    4.021,    4.021,   4.021,   4.021,   4.021,   4.021,   4.021,
     6    4.026,    4.026,   4.026,   4.026,   4.026,   4.026,   4.026,
     7    4.032,    4.032,   4.032,   4.032,   4.032,   4.032,   4.032,
     8    4.039,    4.039,   4.039,   4.039,   4.039,   4.039,   4.039,
     9    4.046,    4.046,   4.046,   4.046,   4.046,   4.046,   4.046/
      data pf364/
     1    4.055,    4.055,   4.055,   4.055,   4.055,   4.055,   4.055,
     2    4.069,    4.069,   4.069,   4.069,   4.069,   4.069,   4.069,
     3    4.087,    4.087,   4.087,   4.087,   4.087,   4.087,   4.087,
     4    4.107,    4.107,   4.107,   4.107,   4.107,   4.107,   4.107,
     5    4.130,    4.130,   4.130,   4.130,   4.130,   4.130,   4.130,
     6    4.156,    4.156,   4.156,   4.156,   4.156,   4.156,   4.156,
     7    4.185,    4.185,   4.185,   4.185,   4.185,   4.185,   4.185,
     8    4.217,    4.217,   4.217,   4.217,   4.217,   4.217,   4.217,
     9    4.252,    4.252,   4.252,   4.252,   4.252,   4.252,   4.252/
      data pf365/
     1    4.289,    4.289,   4.289,   4.289,   4.289,   4.289,   4.289,
     2    4.329,    4.329,   4.329,   4.329,   4.329,   4.329,   4.329,
     3    4.400,    4.400,   4.400,   4.400,   4.400,   4.400,   4.400,
     4    4.477,    4.477,   4.477,   4.477,   4.477,   4.477,   4.477,
     5    4.557,    4.557,   4.557,   4.557,   4.557,   4.557,   4.557,
     6    4.640,    4.640,   4.640,   4.640,   4.640,   4.640,   4.640,
     7    4.725,    4.725,   4.725,   4.725,   4.725,   4.725,   4.725,
     8    4.809,    4.809,   4.809,   4.809,   4.809,   4.809,   4.809,
     9    4.893,    4.893,   4.893,   4.893,   4.893,   4.893,   4.893/
      data pf366/
     1    4.975,    4.975,   4.975,   4.975,   4.975,   4.975,   4.975,
     2    5.054,    5.054,   5.054,   5.054,   5.054,   5.054,   5.054,
     3    5.130,    5.130,   5.130,   5.130,   5.130,   5.130,   5.130,
     4    5.203,    5.203,   5.203,   5.203,   5.203,   5.203,   5.203,
     5    5.271,    5.271,   5.271,   5.271,   5.271,   5.271,   5.271,
     6    5.336,    5.336,   5.336,   5.336,   5.336,   5.336,   5.336,
     7    5.397,    5.397,   5.397,   5.397,   5.397,   5.397,   5.397,
     8    5.458,    5.458,   5.458,   5.458,   5.458,   5.458,   5.458,
     9    5.520,    5.520,   5.520,   5.520,   5.520,   5.520,   5.520/
      data pf367/
     1    5.595,    5.595,   5.595,   5.595,   5.595,   5.595,   5.595,
     2    5.697,    5.697,   5.697,   5.697,   5.697,   5.697,   5.697,
     3    5.856,    5.856,   5.856,   5.856,   5.856,   5.856,   5.856,
     4    6.117,    6.117,   6.117,   6.117,   6.117,   6.117,   6.117,
     5    6.550,    6.550,   6.550,   6.550,   6.550,   6.550,   6.550,
     6    7.257,    7.257,   7.257,   7.257,   7.257,   7.257,   7.257,
     7    8.393,    8.393,   8.393,   8.393,   8.393,   8.393,   8.393,
     8   10.190,   10.190,  10.190,  10.190,  10.190,  10.190,  10.190,
     9   13.002,   13.002,  13.002,  13.002,  13.002,  13.002,  13.002/
      data pf368/
     1   17.374,   17.374,  17.374,  17.374,  17.374,  17.374,  17.374,
     2    5.003,    5.003,   5.003,   5.003,   5.003,   5.003,   5.003,
     3    5.005,    5.005,   5.005,   5.005,   5.005,   5.005,   5.005,
     4    5.006,    5.006,   5.006,   5.006,   5.006,   5.006,   5.006,
     5    5.008,    5.008,   5.008,   5.008,   5.008,   5.008,   5.008,
     6    5.011,    5.011,   5.011,   5.011,   5.011,   5.011,   5.011,
     7    5.014,    5.014,   5.014,   5.014,   5.014,   5.014,   5.014,
     8    5.018,    5.018,   5.018,   5.018,   5.018,   5.018,   5.018,
     9    5.023,    5.023,   5.023,   5.023,   5.023,   5.023,   5.023/
      data pf369/
     1    5.029,    5.029,   5.029,   5.029,   5.029,   5.029,   5.029,
     2    5.036,    5.036,   5.036,   5.036,   5.036,   5.036,   5.036,
     3    5.045,    5.045,   5.045,   5.045,   5.045,   5.045,   5.045,
     4    5.054,    5.054,   5.054,   5.054,   5.054,   5.054,   5.054,
     5    5.066,    5.066,   5.066,   5.066,   5.066,   5.066,   5.066,
     6    5.079,    5.079,   5.079,   5.079,   5.079,   5.079,   5.079,
     7    5.094,    5.094,   5.094,   5.094,   5.094,   5.094,   5.094,
     8    5.112,    5.112,   5.112,   5.112,   5.112,   5.112,   5.112,
     9    5.131,    5.131,   5.131,   5.131,   5.131,   5.131,   5.131/
      data pf370/
     1    5.153,    5.153,   5.153,   5.153,   5.153,   5.153,   5.153,
     2    5.177,    5.177,   5.177,   5.177,   5.177,   5.177,   5.177,
     3    5.203,    5.203,   5.203,   5.203,   5.203,   5.203,   5.203,
     4    5.248,    5.248,   5.248,   5.248,   5.248,   5.248,   5.248,
     5    5.298,    5.298,   5.298,   5.298,   5.298,   5.298,   5.298,
     6    5.355,    5.355,   5.355,   5.355,   5.355,   5.355,   5.355,
     7    5.418,    5.418,   5.418,   5.418,   5.418,   5.418,   5.418,
     8    5.488,    5.488,   5.488,   5.488,   5.488,   5.488,   5.488,
     9    5.563,    5.563,   5.563,   5.563,   5.563,   5.563,   5.563/
      data pf371/
     1    5.646,    5.646,   5.646,   5.646,   5.646,   5.646,   5.646,
     2    5.734,    5.734,   5.734,   5.734,   5.734,   5.734,   5.734,
     3    5.830,    5.830,   5.830,   5.830,   5.830,   5.830,   5.830,
     4    5.931,    5.931,   5.931,   5.931,   5.931,   5.931,   5.931,
     5    6.116,    6.116,   6.116,   6.116,   6.116,   6.116,   6.116,
     6    6.319,    6.319,   6.319,   6.319,   6.319,   6.319,   6.319,
     7    6.542,    6.542,   6.542,   6.542,   6.542,   6.542,   6.542,
     8    6.784,    6.784,   6.784,   6.784,   6.784,   6.784,   6.784,
     9    7.045,    7.045,   7.045,   7.045,   7.045,   7.045,   7.045/
      data pf372/
     1    7.325,    7.325,   7.325,   7.325,   7.325,   7.325,   7.325,
     2    7.622,    7.622,   7.622,   7.622,   7.622,   7.622,   7.622,
     3    7.934,    7.934,   7.934,   7.934,   7.934,   7.934,   7.934,
     4    8.259,    8.259,   8.259,   8.259,   8.259,   8.259,   8.259,
     5    8.595,    8.595,   8.595,   8.595,   8.595,   8.595,   8.595,
     6    8.938,    8.938,   8.938,   8.938,   8.938,   8.938,   8.938,
     7    9.285,    9.285,   9.285,   9.285,   9.285,   9.285,   9.285,
     8    9.635,    9.635,   9.635,   9.635,   9.635,   9.635,   9.635,
     9    9.984,    9.984,   9.984,   9.984,   9.984,   9.984,   9.984/
      data pf373/
     1   10.336,   10.336,  10.336,  10.336,  10.336,  10.336,  10.336,
     2   10.693,   10.693,  10.693,  10.693,  10.693,  10.693,  10.693,
     3   11.069,   11.069,  11.069,  11.069,  11.069,  11.069,  11.069,
     4   11.487,   11.487,  11.487,  11.487,  11.487,  11.487,  11.487,
     5   11.987,   11.987,  11.987,  11.987,  11.987,  11.987,  11.987,
     6   12.633,   12.633,  12.633,  12.633,  12.633,  12.633,  12.633,
     7   13.520,   13.520,  13.520,  13.520,  13.520,  13.520,  13.520,
     8   14.787,   14.787,  14.787,  14.787,  14.787,  14.787,  14.787,
     9   16.631,   16.631,  16.631,  16.631,  16.631,  16.631,  16.631/
      data pf374/
     1   19.338,   19.338,  19.338,  19.338,  19.338,  19.338,  19.338,
     2   23.317,   23.317,  23.317,  23.317,  23.317,  23.317,  23.317,
     3   29.168,   29.168,  29.168,  29.168,  29.168,  29.168,  29.168,
     4   19.692,   19.692,  19.692,  19.692,  19.692,  19.692,  19.692,
     5   19.946,   19.946,  19.946,  19.946,  19.946,  19.946,  19.946,
     6   20.207,   20.207,  20.207,  20.207,  20.207,  20.207,  20.207,
     7   20.476,   20.476,  20.476,  20.476,  20.476,  20.476,  20.476,
     8   20.754,   20.754,  20.754,  20.754,  20.754,  20.754,  20.754,
     9   21.044,   21.044,  21.044,  21.044,  21.044,  21.044,  21.044/
      data pf375/
     1   21.348,   21.348,  21.348,  21.348,  21.348,  21.348,  21.348,
     2   21.667,   21.667,  21.667,  21.667,  21.667,  21.667,  21.667,
     3   22.004,   22.004,  22.004,  22.004,  22.004,  22.004,  22.004,
     4   22.362,   22.362,  22.362,  22.362,  22.362,  22.362,  22.361,
     5   22.742,   22.742,  22.742,  22.742,  22.742,  22.742,  22.742,
     6   23.148,   23.148,  23.148,  23.148,  23.148,  23.148,  23.148,
     7   23.584,   23.584,  23.584,  23.584,  23.584,  23.584,  23.583,
     8   24.052,   24.052,  24.052,  24.052,  24.052,  24.052,  24.051,
     9   24.557,   24.557,  24.557,  24.557,  24.557,  24.557,  24.556/
      data pf376/
     1   25.105,   25.105,  25.105,  25.105,  25.105,  25.104,  25.103,
     2   25.699,   25.699,  25.699,  25.699,  25.699,  25.699,  25.696,
     3   26.347,   26.347,  26.347,  26.347,  26.347,  26.347,  26.341,
     4   27.056,   27.056,  27.056,  27.056,  27.056,  27.056,  27.047,
     5   27.834,   27.834,  27.834,  27.834,  27.834,  27.833,  27.818,
     6   29.153,   29.153,  29.153,  29.153,  29.152,  29.150,  29.120,
     7   30.689,   30.689,  30.688,  30.688,  30.686,  30.681,  30.624,
     8   32.490,   32.490,  32.490,  32.488,  32.484,  32.472,  32.367,
     9   34.619,   34.619,  34.617,  34.612,  34.602,  34.576,  34.389/
      data pf377/
     1   37.152,   37.151,  37.147,  37.136,  37.112,  37.057,  36.737,
     2   40.188,   40.186,  40.176,  40.151,  40.098,  39.989,  39.455,
     3   43.856,   43.851,  43.829,  43.775,  43.664,  43.456,  42.594,
     4   48.322,   48.310,  48.266,  48.154,  47.934,  47.554,  46.202,
     5   53.803,   53.780,  53.693,  53.474,  53.056,  52.388,  50.323,
     6   60.584,   60.539,  60.374,  59.965,  59.205,  58.074,  55.000,
     7   75.805,   75.685,  75.248,  74.190,  72.301,  69.778,  64.126,
     8   98.002,   97.715,  96.675,  94.205,  89.953,  84.791,  75.012,
     9  130.393,  129.764, 127.511, 122.253, 113.490, 103.711,  87.706/
      data pf378/
     1  177.139,  175.874, 171.388, 161.078, 144.378, 127.086, 102.180,
     2  243.218,  240.863, 232.575, 213.785, 184.112, 155.358, 118.325,
     3  334.109,  330.010, 315.685, 283.604, 234.070, 188.811, 135.966,
     4  455.302,  448.584, 425.258, 373.577, 295.363, 227.535, 154.871,
     5  611.734,  601.300, 565.276, 486.228, 368.703, 271.406, 174.770,
     6  807.227,  791.780, 738.715, 623.266, 454.314, 320.086, 195.372,
     7 1044.045, 1022.131, 947.188, 785.378, 551.886, 373.050, 216.382,
     8 1322.623, 1292.694,1190.753, 972.138, 660.596, 429.620, 237.516,
     9 1641.501, 1601.988,1467.884,1182.028, 779.169, 489.011, 258.512/
      data pf379/
     1 1997.454, 1946.842,1775.611,1412.573, 905.981, 550.380, 279.137,
     2 2385.781, 2322.673,2109.771,1660.539,1039.170, 612.874, 299.192,
     3 2800.685, 2723.862,2465.345,1922.183,1176.761, 675.671, 318.517,
     4 3235.699, 3144.159,2836.810,2193.498,1316.775, 738.010, 336.985,
     5 3684.095, 3577.077,3218.484,2470.447,1457.320, 799.216, 354.504,
     6 4139.252, 4016.247,3604.823,2749.161,1596.668, 858.711, 371.014,
     7 4594.948, 4455.693,3990.657,3026.088,1733.297, 916.026, 386.479,
     8 5045.575, 4890.036,4371.358,3298.094,1865.923, 970.793, 400.889,
     9 5486.275, 5314.625,4742.944,3562.522,1993.507,1022.743, 414.254/
      data pf380/
     1 5913.008, 5725.597,5102.129,3817.216,2115.252,1071.699, 426.594,
     2 6322.559, 6119.885,5446.320,4060.508,2230.584,1117.562, 437.947,
     3 6712.506, 6495.185,5773.589,4291.191,2339.131,1160.300, 448.355,
     4 7081.158, 6849.892,6082.609,4508.468,2440.699,1199.938, 457.867,
     5 7427.471, 7183.025,6372.590,4711.909,2535.242,1236.543, 466.539,
     6   28.795,   28.795,  28.795,  28.795,  28.795,  28.795,  28.795,
     7   29.467,   29.467,  29.467,  29.467,  29.467,  29.467,  29.467,
     8   30.148,   30.148,  30.148,  30.148,  30.148,  30.148,  30.148,
     9   30.838,   30.838,  30.838,  30.838,  30.838,  30.838,  30.838/
      data pf381/
     1   31.536,   31.536,  31.536,  31.536,  31.536,  31.536,  31.536,
     2   32.244,   32.244,  32.244,  32.244,  32.244,  32.244,  32.244,
     3   32.960,   32.960,  32.960,  32.960,  32.960,  32.960,  32.960,
     4   33.685,   33.685,  33.685,  33.685,  33.685,  33.685,  33.685,
     5   34.419,   34.419,  34.419,  34.419,  34.419,  34.419,  34.419,
     6   35.163,   35.163,  35.163,  35.163,  35.163,  35.163,  35.163,
     7   35.918,   35.918,  35.918,  35.918,  35.918,  35.918,  35.918,
     8   36.684,   36.684,  36.684,  36.684,  36.684,  36.684,  36.684,
     9   37.463,   37.463,  37.463,  37.463,  37.463,  37.463,  37.463/
      data pf382/
     1   38.257,   38.257,  38.257,  38.257,  38.257,  38.257,  38.257,
     2   39.067,   39.067,  39.067,  39.067,  39.067,  39.067,  39.067,
     3   39.896,   39.896,  39.896,  39.896,  39.896,  39.896,  39.896,
     4   40.746,   40.746,  40.746,  40.746,  40.746,  40.746,  40.746,
     5   41.622,   41.622,  41.622,  41.622,  41.622,  41.622,  41.622,
     6   42.526,   42.526,  42.526,  42.526,  42.526,  42.526,  42.526,
     7   43.464,   43.464,  43.464,  43.464,  43.464,  43.464,  43.464,
     8   44.946,   44.946,  44.946,  44.946,  44.946,  44.946,  44.946,
     9   46.536,   46.536,  46.536,  46.536,  46.536,  46.536,  46.536/
      data pf383/
     1   48.258,   48.258,  48.258,  48.258,  48.258,  48.258,  48.258,
     2   50.140,   50.140,  50.140,  50.140,  50.140,  50.140,  50.140,
     3   52.215,   52.215,  52.215,  52.215,  52.215,  52.215,  52.215,
     4   54.519,   54.519,  54.519,  54.519,  54.519,  54.519,  54.519,
     5   57.093,   57.093,  57.093,  57.093,  57.093,  57.093,  57.093,
     6   59.980,   59.980,  59.980,  59.980,  59.980,  59.980,  59.980,
     7   63.230,   63.230,  63.230,  63.230,  63.230,  63.230,  63.230,
     8   66.899,   66.899,  66.899,  66.899,  66.899,  66.899,  66.898,
     9   74.112,   74.112,  74.112,  74.112,  74.112,  74.111,  74.106/
      data pf384/
     1   83.002,   83.001,  83.001,  83.001,  83.000,  82.995,  82.973,
     2   94.020,   94.020,  94.019,  94.017,  94.011,  93.992,  93.908,
     3  107.809,  107.808, 107.805, 107.798, 107.774, 107.702, 107.427,
     4  125.325,  125.321, 125.311, 125.283, 125.202, 124.972, 124.182,
     5  148.022,  148.009, 147.979, 147.893, 147.652, 146.997, 144.970,
     6  178.080,  178.046, 177.962, 177.730, 177.089, 175.428, 170.728,
     7  218.642,  218.557, 218.348, 217.784, 216.257, 212.447, 202.492,
     8  273.973,  273.782, 273.317, 272.067, 268.757, 260.770, 241.323,
     9  349.483,  349.092, 348.139, 345.601, 339.005, 323.554, 288.212/
      data pf385/
     1  451.527,  450.783, 448.978, 444.209, 432.010, 404.187, 343.967,
     2  586.971,  585.651, 582.463, 574.095, 552.992, 505.991, 409.120,
     3  762.573,  760.375, 755.081, 741.267, 706.876, 631.869, 483.847,
     4  984.275,  980.811, 972.491, 950.899, 897.749, 783.975, 567.932,
     5 1256.523, 1251.328,1238.881,1206.728,1128.386, 963.442, 660.764,
     6 1581.735, 1574.280,1556.456,1510.608,1399.902,1170.229, 761.380,
     7 1960.002, 1949.714,1925.169,1862.267,1711.600,1403.095, 868.532,
     8 2389.037, 2375.329,2342.684,2259.302,2061.006,1659.686, 980.774,
     9 2864.368, 2846.665,2804.572,2697.378,2444.077,1936.729,1096.548/
      data pf386/
     1 3379.718, 3357.483,3304.687,3170.596,2855.531,2230.276,1214.278,
     2 3927.498, 3900.254,3835.646,3671.942,3289.242,2535.981,1332.443,
     3 4499.342, 4466.691,4389.345,4193.779,3738.651,2849.364,1449.637,
     4 5086.626, 5048.257,4957.457,4728.301,4197.145,3166.042,1564.612,
     5 5680.920, 5636.616,5531.863,5267.938,4658.378,3481.920,1676.303,
     6 6274.347, 6223.984,6104.999,5805.663,5116.521,3793.322,1783.843,
     7 6859.847, 6803.388,6670.097,6335.215,5566.429,4097.083,1886.555,
     8   19.261,   19.261,  19.261,  19.261,  19.261,  19.261,  19.261,
     9   19.464,   19.464,  19.464,  19.464,  19.464,  19.464,  19.464/
      data pf387/
     1   19.661,   19.661,  19.661,  19.661,  19.661,  19.661,  19.661,
     2   19.854,   19.854,  19.854,  19.854,  19.854,  19.854,  19.854,
     3   20.041,   20.041,  20.041,  20.041,  20.041,  20.041,  20.041,
     4   20.224,   20.224,  20.224,  20.224,  20.224,  20.224,  20.224,
     5   20.402,   20.402,  20.402,  20.402,  20.402,  20.402,  20.402,
     6   20.575,   20.575,  20.575,  20.575,  20.575,  20.575,  20.575,
     7   20.743,   20.743,  20.743,  20.743,  20.743,  20.743,  20.743,
     8   20.908,   20.908,  20.908,  20.908,  20.908,  20.908,  20.908,
     9   21.068,   21.068,  21.068,  21.068,  21.068,  21.068,  21.068/
      data pf388/
     1   21.225,   21.225,  21.225,  21.225,  21.225,  21.225,  21.225,
     2   21.378,   21.378,  21.378,  21.378,  21.378,  21.378,  21.378,
     3   21.530,   21.530,  21.530,  21.530,  21.530,  21.530,  21.530,
     4   21.680,   21.680,  21.680,  21.680,  21.680,  21.680,  21.680,
     5   21.830,   21.830,  21.830,  21.830,  21.830,  21.830,  21.830,
     6   21.981,   21.981,  21.981,  21.981,  21.981,  21.981,  21.981,
     7   22.135,   22.135,  22.135,  22.135,  22.135,  22.135,  22.135,
     8   22.294,   22.294,  22.294,  22.294,  22.294,  22.294,  22.294,
     9   22.459,   22.459,  22.459,  22.459,  22.459,  22.459,  22.459/
      data pf389/
     1   22.726,   22.726,  22.726,  22.726,  22.726,  22.726,  22.726,
     2   23.024,   23.024,  23.024,  23.024,  23.024,  23.024,  23.024,
     3   23.364,   23.364,  23.364,  23.364,  23.364,  23.364,  23.364,
     4   23.759,   23.759,  23.759,  23.759,  23.759,  23.759,  23.759,
     5   24.225,   24.225,  24.225,  24.225,  24.225,  24.225,  24.225,
     6   24.777,   24.777,  24.777,  24.777,  24.777,  24.777,  24.777,
     7   25.431,   25.431,  25.431,  25.431,  25.431,  25.431,  25.431,
     8   26.206,   26.206,  26.206,  26.206,  26.206,  26.206,  26.206,
     9   27.118,   27.118,  27.118,  27.118,  27.118,  27.118,  27.118/
      data pf390/
     1   28.186,   28.186,  28.186,  28.186,  28.186,  28.186,  28.186,
     2   30.360,   30.360,  30.360,  30.360,  30.360,  30.360,  30.360,
     3   33.097,   33.097,  33.097,  33.097,  33.097,  33.097,  33.097,
     4   36.482,   36.482,  36.482,  36.482,  36.482,  36.482,  36.482,
     5   40.609,   40.609,  40.609,  40.609,  40.609,  40.609,  40.609,
     6   45.595,   45.595,  45.595,  45.595,  45.595,  45.595,  45.595,
     7   51.603,   51.603,  51.603,  51.603,  51.603,  51.603,  51.602,
     8   58.864,   58.864,  58.864,  58.864,  58.863,  58.862,  58.859,
     9   67.724,   67.724,  67.723,  67.723,  67.720,  67.716,  67.702/
      data pf391/
     1   78.710,   78.708,  78.705,  78.703,  78.690,  78.673,  78.620,
     2   92.634,   92.626,  92.616,  92.607,  92.558,  92.497,  92.314,
     3  110.769,  110.742, 110.706, 110.678, 110.517, 110.321, 109.772,
     4  135.082,  135.003, 134.897, 134.814, 134.347, 133.797, 132.334,
     5  168.539,  168.329, 168.052, 167.835, 166.633, 165.251, 161.745,
     6  215.393,  214.896, 214.239, 213.730, 210.934, 207.795, 200.156,
     7  281.384,  280.310, 278.897, 277.806, 271.875, 265.351, 250.055,
     8  373.723,  371.591, 368.790, 366.640, 355.047, 342.525, 314.118,
     9  500.793,  496.864, 491.713, 487.776, 466.708, 444.317, 394.990/
      data pf392/
     1  671.547,  664.773, 655.906, 649.156, 613.275, 575.691, 495.019,
     2  894.678,  883.673, 869.285, 858.370, 800.700, 741.067, 615.993,
     3 1177.698, 1160.735,1138.586,1121.837,1033.806, 943.819, 758.925,
     4 1526.072, 1501.128,1468.594,1444.062,1315.727,1185.876, 923.911,
     5 1942.576, 1907.403,1861.573,1827.101,1647.523,1467.471,1110.095,
     6 2426.952, 2379.173,2316.973,2270.293,2028.027,1787.082,1315.723,
     7 2975.893, 2913.118,2831.460,2770.299,2453.929,2141.550,1538.276,
     8 3583.340, 3503.275,3399.197,3321.382,2920.061,2526.343,1774.664,
     9 4240.993, 4141.541,4012.342,3915.899,3419.813,2935.906,2021.435/
      data pf393/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     5    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     8    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     9    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000/
      data pf394/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     5    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     8    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     9    6.002,    6.002,   6.002,   6.002,   6.002,   6.002,   6.002/
      data pf395/
     1    6.003,    6.003,   6.003,   6.003,   6.003,   6.003,   6.003,
     2    6.004,    6.004,   6.004,   6.004,   6.004,   6.004,   6.004,
     3    6.008,    6.008,   6.008,   6.008,   6.008,   6.008,   6.008,
     4    6.015,    6.015,   6.015,   6.015,   6.015,   6.015,   6.015,
     5    6.026,    6.026,   6.026,   6.026,   6.026,   6.026,   6.026,
     6    6.043,    6.043,   6.043,   6.043,   6.043,   6.043,   6.043,
     7    6.071,    6.071,   6.071,   6.071,   6.071,   6.071,   6.071,
     8    6.114,    6.114,   6.114,   6.114,   6.114,   6.114,   6.114,
     9    6.176,    6.176,   6.176,   6.176,   6.176,   6.176,   6.176/
      data pf396/
     1    6.267,    6.267,   6.267,   6.267,   6.267,   6.267,   6.267,
     2    6.394,    6.394,   6.394,   6.394,   6.394,   6.394,   6.394,
     3    6.569,    6.569,   6.569,   6.569,   6.569,   6.569,   6.569,
     4    7.005,    7.005,   7.005,   7.005,   7.005,   7.005,   7.005,
     5    7.684,    7.684,   7.684,   7.684,   7.684,   7.684,   7.684,
     6    8.697,    8.697,   8.697,   8.697,   8.697,   8.697,   8.697,
     7   10.140,   10.140,  10.140,  10.140,  10.140,  10.140,  10.140,
     8   12.118,   12.118,  12.118,  12.118,  12.118,  12.118,  12.118,
     9   14.733,   14.733,  14.733,  14.733,  14.733,  14.733,  14.733/
      data pf397/
     1   18.079,   18.079,  18.079,  18.079,  18.079,  18.079,  18.079,
     2   22.240,   22.240,  22.240,  22.240,  22.240,  22.240,  22.240,
     3   27.293,   27.293,  27.293,  27.293,  27.293,  27.293,  27.293,
     4   33.317,   33.317,  33.317,  33.317,  33.317,  33.316,  33.316,
     5   40.419,   40.419,  40.419,  40.419,  40.418,  40.418,  40.418,
     6   48.781,   48.780,  48.780,  48.780,  48.779,  48.777,  48.773,
     7   58.739,   58.738,  58.737,  58.733,  58.730,  58.722,  58.701,
     8   70.943,   70.937,  70.931,  70.916,  70.902,  70.863,  70.773,
     9   86.649,   86.626,  86.603,  86.542,  86.490,  86.338,  86.006/
      data pf398/
     1  108.252,  108.172, 108.095, 107.889, 107.714, 107.209, 106.144,
     2  140.109,  139.870, 139.641, 139.028, 138.511, 137.042, 134.035,
     3  189.665,  189.033, 188.425, 186.807, 185.453, 181.642, 174.059,
     4  268.697,  267.191, 265.743, 261.899, 258.702, 249.795, 232.499,
     5  394.354,  391.085, 387.948, 379.636, 372.763, 353.778, 317.712,
     6  589.551,  583.033, 576.782, 560.258, 546.660, 509.392, 439.960,
     7  882.376,  870.316, 858.762, 828.276, 803.299, 735.314, 610.833,
     8 1304.331, 1283.464,1263.487,1210.863,1167.920,1051.749, 842.299,
     9 1887.589, 1853.575,1821.030,1735.431,1665.822,1478.548,1145.514/
      data pf399/
     1 2661.691, 2609.112,2558.835,2426.773,2319.714,2033.093,1529.593,
     2 3650.253, 3572.739,3498.656,3304.293,3147.168,2728.338,2000.570,
     3   15.218,   15.218,  15.218,  15.218,  15.218,  15.218,  15.218,
     4   15.531,   15.531,  15.531,  15.531,  15.531,  15.531,  15.531,
     5   15.839,   15.839,  15.839,  15.839,  15.839,  15.839,  15.839,
     6   16.142,   16.142,  16.142,  16.142,  16.142,  16.142,  16.142,
     7   16.438,   16.438,  16.438,  16.438,  16.438,  16.438,  16.438,
     8   16.728,   16.728,  16.728,  16.728,  16.728,  16.728,  16.728,
     9   17.012,   17.012,  17.012,  17.012,  17.012,  17.012,  17.012/
      data pf400/
     1   17.290,   17.290,  17.290,  17.290,  17.290,  17.290,  17.290,
     2   17.561,   17.561,  17.561,  17.561,  17.561,  17.561,  17.561,
     3   17.826,   17.826,  17.826,  17.826,  17.826,  17.826,  17.826,
     4   18.084,   18.084,  18.084,  18.084,  18.084,  18.084,  18.084,
     5   18.336,   18.336,  18.336,  18.336,  18.336,  18.336,  18.336,
     6   18.582,   18.582,  18.582,  18.582,  18.582,  18.582,  18.582,
     7   18.821,   18.821,  18.821,  18.821,  18.821,  18.821,  18.821,
     8   19.054,   19.054,  19.054,  19.054,  19.054,  19.054,  19.054,
     9   19.281,   19.281,  19.281,  19.281,  19.281,  19.281,  19.281/
      data pf401/
     1   19.503,   19.503,  19.503,  19.503,  19.503,  19.503,  19.503,
     2   19.721,   19.721,  19.721,  19.721,  19.721,  19.721,  19.721,
     3   19.935,   19.935,  19.935,  19.935,  19.935,  19.935,  19.935,
     4   20.146,   20.146,  20.146,  20.146,  20.146,  20.146,  20.146,
     5   20.460,   20.460,  20.460,  20.460,  20.460,  20.460,  20.460,
     6   20.775,   20.775,  20.775,  20.775,  20.775,  20.775,  20.775,
     7   21.098,   21.098,  21.098,  21.098,  21.098,  21.098,  21.098,
     8   21.437,   21.437,  21.437,  21.437,  21.437,  21.437,  21.437,
     9   21.801,   21.801,  21.801,  21.801,  21.801,  21.801,  21.801/
      data pf402/
     1   22.203,   22.203,  22.203,  22.203,  22.203,  22.203,  22.203,
     2   22.654,   22.654,  22.654,  22.654,  22.654,  22.654,  22.654,
     3   23.169,   23.169,  23.169,  23.169,  23.169,  23.169,  23.169,
     4   23.763,   23.763,  23.763,  23.763,  23.763,  23.763,  23.763,
     5   24.453,   24.453,  24.453,  24.453,  24.453,  24.453,  24.453,
     6   25.859,   25.859,  25.859,  25.859,  25.859,  25.859,  25.859,
     7   27.649,   27.649,  27.649,  27.649,  27.649,  27.649,  27.649,
     8   29.893,   29.893,  29.893,  29.893,  29.893,  29.893,  29.893,
     9   32.647,   32.647,  32.647,  32.647,  32.647,  32.647,  32.647/
      data pf403/
     1   35.951,   35.951,  35.951,  35.951,  35.951,  35.951,  35.951,
     2   39.830,   39.830,  39.830,  39.830,  39.830,  39.830,  39.830,
     3   44.287,   44.287,  44.287,  44.287,  44.287,  44.287,  44.287,
     4   49.307,   49.307,  49.307,  49.307,  49.307,  49.307,  49.307,
     5   54.859,   54.859,  54.859,  54.859,  54.859,  54.859,  54.859,
     6   60.898,   60.898,  60.898,  60.898,  60.898,  60.898,  60.898,
     7   67.374,   67.374,  67.374,  67.374,  67.374,  67.374,  67.374,
     8   74.245,   74.245,  74.245,  74.245,  74.245,  74.245,  74.245,
     9   81.494,   81.494,  81.494,  81.494,  81.494,  81.494,  81.494/
      data pf404/
     1   89.160,   89.160,  89.160,  89.159,  89.159,  89.159,  89.158,
     2   97.379,   97.379,  97.379,  97.378,  97.378,  97.375,  97.371,
     3  106.480,  106.480, 106.479, 106.475, 106.472, 106.459, 106.435,
     4  117.148,  117.148, 117.141, 117.123, 117.110, 117.056, 116.955,
     5  130.762,  130.762, 130.733, 130.664, 130.616, 130.416, 130.052,
     6  149.982,  149.981, 149.888, 149.660, 149.506, 148.862, 147.721,
     7  179.660,  179.657, 179.389, 178.734, 178.292, 176.466, 173.310,
     8  228.024,  228.018, 227.327, 225.646, 224.519, 219.898, 212.084,
     9  307.932,  307.917, 306.314, 302.420, 299.823, 289.250, 271.719/
      data pf405/
     1  437.809,  437.777, 434.385, 426.152, 420.687, 398.576, 362.553,
     2  641.858,  641.796, 635.178, 619.132, 608.526, 565.851, 497.404,
     3  949.185,  949.072, 937.065, 907.982, 888.831, 812.151, 690.860,
     4 1391.783, 1391.591,1371.175,1321.762,1289.334,1160.059, 958.090,
     5   14.270,   14.270,  14.270,  14.270,  14.270,  14.270,  14.270,
     6   14.631,   14.631,  14.631,  14.631,  14.631,  14.631,  14.631,
     7   14.992,   14.992,  14.992,  14.992,  14.992,  14.992,  14.992,
     8   15.350,   15.350,  15.350,  15.350,  15.350,  15.350,  15.350,
     9   15.707,   15.707,  15.707,  15.707,  15.707,  15.707,  15.707/
      data pf406/
     1   16.061,   16.061,  16.061,  16.061,  16.061,  16.061,  16.061,
     2   16.411,   16.411,  16.411,  16.411,  16.411,  16.411,  16.411,
     3   16.759,   16.759,  16.759,  16.759,  16.759,  16.759,  16.759,
     4   17.102,   17.102,  17.102,  17.102,  17.102,  17.102,  17.102,
     5   17.441,   17.441,  17.441,  17.441,  17.441,  17.441,  17.441,
     6   17.776,   17.776,  17.776,  17.776,  17.776,  17.776,  17.776,
     7   18.106,   18.106,  18.106,  18.106,  18.106,  18.106,  18.106,
     8   18.432,   18.432,  18.432,  18.432,  18.432,  18.432,  18.432,
     9   18.753,   18.753,  18.753,  18.753,  18.753,  18.753,  18.753/
      data pf407/
     1   19.069,   19.069,  19.069,  19.069,  19.069,  19.069,  19.069,
     2   19.382,   19.382,  19.382,  19.382,  19.382,  19.382,  19.382,
     3   19.691,   19.691,  19.691,  19.691,  19.691,  19.691,  19.691,
     4   19.997,   19.997,  19.997,  19.997,  19.997,  19.997,  19.997,
     5   20.300,   20.300,  20.300,  20.300,  20.300,  20.300,  20.300,
     6   20.602,   20.602,  20.602,  20.602,  20.602,  20.602,  20.602,
     7   21.056,   21.056,  21.056,  21.056,  21.056,  21.056,  21.056,
     8   21.512,   21.512,  21.512,  21.512,  21.512,  21.512,  21.512,
     9   21.979,   21.979,  21.979,  21.979,  21.979,  21.979,  21.979/
      data pf408/
     1   22.461,   22.461,  22.461,  22.461,  22.461,  22.461,  22.461,
     2   22.965,   22.965,  22.965,  22.965,  22.965,  22.965,  22.965,
     3   23.500,   23.500,  23.500,  23.500,  23.500,  23.500,  23.500,
     4   24.073,   24.073,  24.073,  24.073,  24.073,  24.073,  24.073,
     5   24.693,   24.693,  24.693,  24.693,  24.693,  24.693,  24.693,
     6   25.368,   25.368,  25.368,  25.368,  25.368,  25.368,  25.368,
     7   26.107,   26.107,  26.107,  26.107,  26.107,  26.107,  26.107,
     8   27.499,   27.499,  27.499,  27.499,  27.499,  27.499,  27.499,
     9   29.119,   29.119,  29.119,  29.119,  29.119,  29.119,  29.119/
      data pf409/
     1   30.989,   30.989,  30.989,  30.989,  30.989,  30.989,  30.989,
     2   33.124,   33.124,  33.124,  33.124,  33.124,  33.124,  33.124,
     3   35.525,   35.525,  35.525,  35.525,  35.525,  35.525,  35.525,
     4   38.187,   38.187,  38.187,  38.187,  38.187,  38.187,  38.187,
     5   41.093,   41.093,  41.093,  41.093,  41.093,  41.093,  41.093,
     6   44.218,   44.218,  44.218,  44.218,  44.218,  44.218,  44.218,
     7   47.531,   47.531,  47.531,  47.531,  47.531,  47.531,  47.531,
     8   50.998,   50.998,  50.998,  50.998,  50.998,  50.998,  50.998,
     9   54.581,   54.581,  54.581,  54.581,  54.581,  54.581,  54.581/
      data pf410/
     1   58.240,   58.240,  58.240,  58.240,  58.240,  58.240,  58.240,
     2   61.941,   61.941,  61.941,  61.941,  61.941,  61.941,  61.941,
     3   65.657,   65.657,  65.657,  65.657,  65.657,  65.657,  65.657,
     4   69.379,   69.379,  69.379,  69.379,  69.379,  69.379,  69.379,
     5   73.126,   73.126,  73.126,  73.126,  73.126,  73.126,  73.126,
     6   76.978,   76.978,  76.978,  76.978,  76.977,  76.977,  76.977,
     7   81.117,   81.117,  81.117,  81.117,  81.117,  81.117,  81.114,
     8   85.922,   85.921,  85.921,  85.921,  85.920,  85.919,  85.903,
     9   92.113,   92.113,  92.112,  92.111,  92.108,  92.103,  92.042/
      data pf411/
     1  101.024,  101.023, 101.021, 101.016, 101.006, 100.987, 100.785,
     2  114.980,  114.978, 114.970, 114.956, 114.925, 114.869, 114.276,
     3  137.804,  137.798, 137.778, 137.739, 137.655, 137.505, 135.955,
     4  175.347,  175.331, 175.283, 175.190, 174.988, 174.630, 170.978,
     5  235.900,  235.866, 235.759, 235.555, 235.113, 234.335, 226.500,
     6  330.304,  330.235, 330.019, 329.607, 328.721, 327.165, 311.692,
     7   10.200,   10.200,  10.200,  10.200,  10.200,  10.200,  10.200,
     8   10.448,   10.448,  10.448,  10.448,  10.448,  10.448,  10.448,
     9   10.698,   10.698,  10.698,  10.698,  10.698,  10.698,  10.698/
      data pf412/
     1   10.949,   10.949,  10.949,  10.949,  10.949,  10.949,  10.949,
     2   11.201,   11.201,  11.201,  11.201,  11.201,  11.201,  11.201,
     3   11.453,   11.453,  11.453,  11.453,  11.453,  11.453,  11.453,
     4   11.705,   11.705,  11.705,  11.705,  11.705,  11.705,  11.705,
     5   11.956,   11.956,  11.956,  11.956,  11.956,  11.956,  11.956,
     6   12.207,   12.207,  12.207,  12.207,  12.207,  12.207,  12.207,
     7   12.456,   12.456,  12.456,  12.456,  12.456,  12.456,  12.456,
     8   12.704,   12.704,  12.704,  12.704,  12.704,  12.704,  12.704,
     9   12.951,   12.951,  12.951,  12.951,  12.951,  12.951,  12.951/
      data pf413/
     1   13.195,   13.195,  13.195,  13.195,  13.195,  13.195,  13.195,
     2   13.437,   13.437,  13.437,  13.437,  13.437,  13.437,  13.437,
     3   13.676,   13.676,  13.676,  13.676,  13.676,  13.676,  13.676,
     4   13.914,   13.914,  13.914,  13.914,  13.914,  13.914,  13.914,
     5   14.149,   14.149,  14.149,  14.149,  14.149,  14.149,  14.149,
     6   14.382,   14.382,  14.382,  14.382,  14.382,  14.382,  14.382,
     7   14.613,   14.613,  14.613,  14.613,  14.613,  14.613,  14.613,
     8   14.843,   14.843,  14.843,  14.843,  14.843,  14.843,  14.843,
     9   15.185,   15.185,  15.185,  15.185,  15.185,  15.185,  15.185/
      data pf414/
     1   15.525,   15.525,  15.525,  15.525,  15.525,  15.525,  15.525,
     2   15.865,   15.865,  15.865,  15.865,  15.865,  15.865,  15.865,
     3   16.208,   16.208,  16.208,  16.208,  16.208,  16.208,  16.208,
     4   16.556,   16.556,  16.556,  16.556,  16.556,  16.556,  16.556,
     5   16.910,   16.910,  16.910,  16.910,  16.910,  16.910,  16.910,
     6   17.274,   17.274,  17.274,  17.274,  17.274,  17.274,  17.274,
     7   17.649,   17.649,  17.649,  17.649,  17.649,  17.649,  17.649,
     8   18.039,   18.039,  18.039,  18.039,  18.039,  18.039,  18.039,
     9   18.446,   18.446,  18.446,  18.446,  18.446,  18.446,  18.446/
      data pf415/
     1   19.165,   19.165,  19.165,  19.165,  19.165,  19.165,  19.165,
     2   19.942,   19.942,  19.942,  19.942,  19.942,  19.942,  19.942,
     3   20.780,   20.780,  20.780,  20.780,  20.780,  20.780,  20.780,
     4   21.679,   21.679,  21.679,  21.679,  21.679,  21.679,  21.679,
     5   22.635,   22.635,  22.635,  22.635,  22.635,  22.635,  22.635,
     6   23.641,   23.641,  23.641,  23.641,  23.641,  23.641,  23.641,
     7   24.689,   24.689,  24.689,  24.689,  24.689,  24.689,  24.689,
     8   25.767,   25.767,  25.767,  25.767,  25.767,  25.767,  25.767,
     9   26.865,   26.865,  26.865,  26.865,  26.865,  26.865,  26.865/
      data pf416/
     1   27.971,   27.971,  27.971,  27.971,  27.971,  27.971,  27.971,
     2   29.072,   29.072,  29.072,  29.072,  29.072,  29.072,  29.072,
     3   30.159,   30.159,  30.159,  30.159,  30.159,  30.159,  30.159,
     4   31.222,   31.222,  31.222,  31.222,  31.222,  31.222,  31.222,
     5   32.255,   32.255,  32.255,  32.255,  32.255,  32.255,  32.255,
     6   33.255,   33.255,  33.255,  33.255,  33.255,  33.255,  33.255,
     7   34.228,   34.228,  34.228,  34.228,  34.228,  34.228,  34.228,
     8   35.198,   35.198,  35.198,  35.198,  35.198,  35.198,  35.198,
     9   36.217,   36.217,  36.217,  36.217,  36.217,  36.217,  36.217/
      data pf417/
     1   37.388,   37.388,  37.388,  37.388,  37.388,  37.388,  37.388,
     2   38.889,   38.889,  38.889,  38.889,  38.889,  38.889,  38.889,
     3   41.016,   41.016,  41.016,  41.016,  41.016,  41.016,  41.016,
     4   44.230,   44.230,  44.230,  44.230,  44.230,  44.229,  44.228,
     5   49.242,   49.242,  49.242,  49.242,  49.242,  49.241,  49.238,
     6   57.126,   57.126,  57.126,  57.126,  57.126,  57.124,  57.115,
     7   69.461,   69.461,  69.461,  69.461,  69.461,  69.455,  69.431,
     8   88.474,   88.474,  88.474,  88.474,  88.474,  88.459,  88.400,
     9    5.695,    5.695,   5.695,   5.695,   5.695,   5.695,   5.695/
      data pf418/
     1    5.794,    5.794,   5.794,   5.794,   5.794,   5.794,   5.794,
     2    5.894,    5.894,   5.894,   5.894,   5.894,   5.894,   5.894,
     3    5.995,    5.995,   5.995,   5.995,   5.995,   5.995,   5.995,
     4    6.096,    6.096,   6.096,   6.096,   6.096,   6.096,   6.096,
     5    6.198,    6.198,   6.198,   6.198,   6.198,   6.198,   6.198,
     6    6.299,    6.299,   6.299,   6.299,   6.299,   6.299,   6.299,
     7    6.401,    6.401,   6.401,   6.401,   6.401,   6.401,   6.401,
     8    6.502,    6.502,   6.502,   6.502,   6.502,   6.502,   6.502,
     9    6.602,    6.602,   6.602,   6.602,   6.602,   6.602,   6.602/
      data pf419/
     1    6.702,    6.702,   6.702,   6.702,   6.702,   6.702,   6.702,
     2    6.801,    6.801,   6.801,   6.801,   6.801,   6.801,   6.801,
     3    6.899,    6.899,   6.899,   6.899,   6.899,   6.899,   6.899,
     4    6.995,    6.995,   6.995,   6.995,   6.995,   6.995,   6.995,
     5    7.090,    7.090,   7.090,   7.090,   7.090,   7.090,   7.090,
     6    7.184,    7.184,   7.184,   7.184,   7.184,   7.184,   7.184,
     7    7.276,    7.276,   7.276,   7.276,   7.276,   7.276,   7.276,
     8    7.366,    7.366,   7.366,   7.366,   7.366,   7.366,   7.366,
     9    7.455,    7.455,   7.455,   7.455,   7.455,   7.455,   7.455/
      data pf420/
     1    7.542,    7.542,   7.542,   7.542,   7.542,   7.542,   7.542,
     2    7.669,    7.669,   7.669,   7.669,   7.669,   7.669,   7.669,
     3    7.791,    7.791,   7.791,   7.791,   7.791,   7.791,   7.791,
     4    7.909,    7.909,   7.909,   7.909,   7.909,   7.909,   7.909,
     5    8.023,    8.023,   8.023,   8.023,   8.023,   8.023,   8.023,
     6    8.131,    8.131,   8.131,   8.131,   8.131,   8.131,   8.131,
     7    8.236,    8.236,   8.236,   8.236,   8.236,   8.236,   8.236,
     8    8.335,    8.335,   8.335,   8.335,   8.335,   8.335,   8.335,
     9    8.430,    8.430,   8.430,   8.430,   8.430,   8.430,   8.430/
      data pf421/
     1    8.521,    8.521,   8.521,   8.521,   8.521,   8.521,   8.521,
     2    8.607,    8.607,   8.607,   8.607,   8.607,   8.607,   8.607,
     3    8.741,    8.741,   8.741,   8.741,   8.741,   8.741,   8.741,
     4    8.864,    8.864,   8.864,   8.864,   8.864,   8.864,   8.864,
     5    8.977,    8.977,   8.977,   8.977,   8.977,   8.977,   8.977,
     6    9.079,    9.079,   9.079,   9.079,   9.079,   9.079,   9.079,
     7    9.172,    9.172,   9.172,   9.172,   9.172,   9.172,   9.172,
     8    9.256,    9.256,   9.256,   9.256,   9.256,   9.256,   9.256,
     9    9.332,    9.332,   9.332,   9.332,   9.332,   9.332,   9.332/
      data pf422/
     1    9.401,    9.401,   9.401,   9.401,   9.401,   9.401,   9.401,
     2    9.463,    9.463,   9.463,   9.463,   9.463,   9.463,   9.463,
     3    9.519,    9.519,   9.519,   9.519,   9.519,   9.519,   9.519,
     4    9.570,    9.570,   9.570,   9.570,   9.570,   9.570,   9.570,
     5    9.615,    9.615,   9.615,   9.615,   9.615,   9.615,   9.615,
     6    9.656,    9.656,   9.656,   9.656,   9.656,   9.656,   9.656,
     7    9.693,    9.693,   9.693,   9.693,   9.693,   9.693,   9.693,
     8    9.729,    9.729,   9.729,   9.729,   9.729,   9.729,   9.729,
     9    9.767,    9.767,   9.767,   9.767,   9.767,   9.767,   9.767/
      data pf423/
     1    9.817,    9.817,   9.817,   9.817,   9.817,   9.817,   9.817,
     2    9.899,    9.899,   9.899,   9.899,   9.899,   9.899,   9.899,
     3   10.047,   10.047,  10.047,  10.047,  10.047,  10.047,  10.047,
     4   10.319,   10.319,  10.319,  10.319,  10.319,  10.319,  10.319,
     5   10.801,   10.801,  10.801,  10.801,  10.801,  10.801,  10.801,
     6   11.615,   11.615,  11.615,  11.615,  11.615,  11.615,  11.615,
     7   12.918,   12.918,  12.918,  12.918,  12.918,  12.918,  12.918,
     8   14.914,   14.914,  14.914,  14.914,  14.914,  14.914,  14.914,
     9   17.857,   17.857,  17.857,  17.857,  17.857,  17.857,  17.857/
      data pf424/
     1   22.076,   22.076,  22.076,  22.076,  22.076,  22.076,  22.076,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf425/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf426/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf427/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf428/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf429/
     1    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     2    1.003,    1.003,   1.003,   1.003,   1.003,   1.003,   1.003,
     3    1.008,    1.008,   1.008,   1.008,   1.008,   1.008,   1.008,
     4    1.021,    1.021,   1.021,   1.021,   1.021,   1.021,   1.021,
     5    1.049,    1.049,   1.049,   1.049,   1.049,   1.049,   1.049,
     6    1.107,    1.107,   1.107,   1.107,   1.107,   1.107,   1.107,
     7    1.217,    1.217,   1.217,   1.217,   1.217,   1.217,   1.217,
     8    1.413,    1.413,   1.413,   1.413,   1.413,   1.413,   1.413,
     9    1.748,    1.748,   1.748,   1.748,   1.748,   1.748,   1.748/
      data pf430/
     1    2.305,    2.305,   2.305,   2.305,   2.305,   2.305,   2.305,
     2    3.213,    3.213,   3.213,   3.213,   3.213,   3.213,   3.213,
     3    4.669,    4.669,   4.669,   4.669,   4.669,   4.669,   4.669,
     4    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     5    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     6    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     7    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     8    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000,
     9    4.000,    4.000,   4.000,   4.000,   4.000,   4.000,   4.000/
      data pf431/
     1    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     2    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     3    4.001,    4.001,   4.001,   4.001,   4.001,   4.001,   4.001,
     4    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     5    4.002,    4.002,   4.002,   4.002,   4.002,   4.002,   4.002,
     6    4.003,    4.003,   4.003,   4.003,   4.003,   4.003,   4.003,
     7    4.004,    4.004,   4.004,   4.004,   4.004,   4.004,   4.004,
     8    4.005,    4.005,   4.005,   4.005,   4.005,   4.005,   4.005,
     9    4.007,    4.007,   4.007,   4.007,   4.007,   4.007,   4.007/
      data pf432/
     1    4.009,    4.009,   4.009,   4.009,   4.009,   4.009,   4.009,
     2    4.011,    4.011,   4.011,   4.011,   4.011,   4.011,   4.011,
     3    4.014,    4.014,   4.014,   4.014,   4.014,   4.014,   4.014,
     4    4.018,    4.018,   4.018,   4.018,   4.018,   4.018,   4.018,
     5    4.022,    4.022,   4.022,   4.022,   4.022,   4.022,   4.022,
     6    4.030,    4.030,   4.030,   4.030,   4.030,   4.030,   4.030,
     7    4.040,    4.040,   4.040,   4.040,   4.040,   4.040,   4.040,
     8    4.051,    4.051,   4.051,   4.051,   4.051,   4.051,   4.051,
     9    4.066,    4.066,   4.066,   4.066,   4.066,   4.066,   4.066/
      data pf433/
     1    4.083,    4.083,   4.083,   4.083,   4.083,   4.083,   4.083,
     2    4.102,    4.102,   4.102,   4.102,   4.102,   4.102,   4.102,
     3    4.125,    4.125,   4.125,   4.125,   4.125,   4.125,   4.125,
     4    4.150,    4.150,   4.150,   4.150,   4.150,   4.150,   4.150,
     5    4.178,    4.178,   4.178,   4.178,   4.178,   4.178,   4.178,
     6    4.209,    4.209,   4.209,   4.209,   4.209,   4.209,   4.209,
     7    4.268,    4.268,   4.268,   4.268,   4.268,   4.268,   4.268,
     8    4.333,    4.333,   4.333,   4.333,   4.333,   4.333,   4.333,
     9    4.405,    4.405,   4.405,   4.405,   4.405,   4.405,   4.405/
      data pf434/
     1    4.482,    4.482,   4.482,   4.482,   4.482,   4.482,   4.482,
     2    4.562,    4.562,   4.562,   4.562,   4.562,   4.562,   4.562,
     3    4.645,    4.645,   4.645,   4.645,   4.645,   4.645,   4.645,
     4    4.730,    4.730,   4.730,   4.730,   4.730,   4.730,   4.730,
     5    4.815,    4.815,   4.815,   4.815,   4.815,   4.815,   4.815,
     6    4.898,    4.898,   4.898,   4.898,   4.898,   4.898,   4.898,
     7    4.980,    4.980,   4.980,   4.980,   4.980,   4.980,   4.980,
     8    5.059,    5.059,   5.059,   5.059,   5.059,   5.059,   5.059,
     9    5.135,    5.135,   5.135,   5.135,   5.135,   5.135,   5.135/
      data pf435/
     1    5.207,    5.207,   5.207,   5.207,   5.207,   5.207,   5.207,
     2    5.276,    5.276,   5.276,   5.276,   5.276,   5.276,   5.276,
     3    5.343,    5.343,   5.343,   5.343,   5.343,   5.343,   5.343,
     4    5.409,    5.409,   5.409,   5.409,   5.409,   5.409,   5.409,
     5    5.481,    5.481,   5.481,   5.481,   5.481,   5.481,   5.481,
     6    5.571,    5.571,   5.571,   5.571,   5.571,   5.571,   5.571,
     7    5.698,    5.698,   5.698,   5.698,   5.698,   5.698,   5.698,
     8    5.899,    5.899,   5.899,   5.899,   5.899,   5.899,   5.899,
     9    6.225,    6.225,   6.225,   6.225,   6.225,   6.225,   6.225/
      data pf436/
     1    6.758,    6.758,   6.758,   6.758,   6.758,   6.758,   6.758,
     2    7.615,    7.615,   7.615,   7.615,   7.615,   7.615,   7.615,
     3    8.967,    8.967,   8.967,   8.967,   8.967,   8.967,   8.967,
     4   11.068,   11.068,  11.068,  11.068,  11.068,  11.068,  11.068,
     5   14.299,   14.299,  14.299,  14.299,  14.299,  14.299,  14.299,
     6   19.784,   19.784,  19.784,  19.784,  19.784,  19.784,  19.784,
     7   20.312,   20.312,  20.312,  20.312,  20.312,  20.312,  20.312,
     8   20.859,   20.859,  20.859,  20.859,  20.859,  20.859,  20.859,
     9   21.426,   21.426,  21.426,  21.426,  21.426,  21.426,  21.426/
      data pf437/
     1   22.013,   22.013,  22.013,  22.013,  22.013,  22.013,  22.013,
     2   22.621,   22.621,  22.621,  22.621,  22.621,  22.621,  22.621,
     3   23.249,   23.249,  23.249,  23.249,  23.249,  23.249,  23.249,
     4   23.898,   23.898,  23.898,  23.898,  23.898,  23.898,  23.898,
     5   24.568,   24.568,  24.568,  24.568,  24.568,  24.568,  24.568,
     6   25.260,   25.260,  25.260,  25.260,  25.260,  25.260,  25.260,
     7   25.974,   25.974,  25.974,  25.974,  25.974,  25.974,  25.974,
     8   26.711,   26.711,  26.711,  26.711,  26.711,  26.711,  26.711,
     9   27.472,   27.472,  27.472,  27.472,  27.472,  27.472,  27.472/
      data pf438/
     1   28.259,   28.259,  28.259,  28.259,  28.259,  28.259,  28.258,
     2   29.073,   29.073,  29.073,  29.073,  29.073,  29.073,  29.071,
     3   29.915,   29.915,  29.915,  29.915,  29.915,  29.915,  29.913,
     4   30.790,   30.790,  30.790,  30.790,  30.790,  30.790,  30.786,
     5   31.699,   31.699,  31.699,  31.699,  31.699,  31.699,  31.693,
     6   32.647,   32.647,  32.647,  32.647,  32.647,  32.647,  32.637,
     7   33.639,   33.639,  33.639,  33.639,  33.639,  33.638,  33.623,
     8   35.222,   35.222,  35.222,  35.221,  35.221,  35.220,  35.190,
     9   36.940,   36.940,  36.940,  36.939,  36.938,  36.935,  36.879/
      data pf439/
     1   38.827,   38.826,  38.825,  38.823,  38.821,  38.813,  38.713,
     2   40.923,   40.921,  40.918,  40.912,  40.908,  40.891,  40.717,
     3   43.283,   43.279,  43.270,  43.257,  43.247,  43.212,  42.919,
     4   45.978,   45.969,  45.948,  45.920,  45.896,  45.826,  45.349,
     5   49.103,   49.083,  49.038,  48.977,  48.929,  48.794,  48.039,
     6   52.786,   52.745,  52.651,  52.526,  52.431,  52.184,  51.021,
     7   57.198,   57.116,  56.930,  56.686,  56.507,  56.070,  54.326,
     8   62.566,   62.410,  62.058,  61.603,  61.280,  60.537,  57.983,
     9   74.490,   74.075,  73.146,  71.974,  71.179,  69.513,  64.926/
      data pf440/
     1   91.899,   90.906,  88.704,  85.980,  84.201,  80.776,  73.002,
     2  117.605,  115.444, 110.690, 104.914, 101.270,  94.752,  82.253,
     3  155.353,  151.030, 141.591, 130.304, 123.396, 111.822,  92.670,
     4  209.746,  201.726, 184.332, 163.824, 151.609, 132.290, 104.188,
     5  285.982,  272.070, 242.077, 207.158, 186.853, 156.342, 116.697,
     6  389.417,  366.688, 317.945, 261.831, 229.891, 184.022, 130.048,
     7  525.025,  489.822, 414.682, 329.040, 281.211, 215.225, 144.061,
     8  696.846,  644.856, 534.351, 409.514, 340.964, 249.700, 158.543,
     9  907.519,  833.923, 678.082, 503.418, 408.937, 287.064, 173.295/
      data pf441/
     1 1157.969, 1057.653, 845.941, 610.325, 484.564, 326.835, 188.123,
     2 1447.303, 1315.095,1036.905, 729.245, 566.971, 368.460, 202.848,
     3 1772.887, 1603.800,1248.955, 858.708, 655.040, 411.352, 217.310,
     4 2130.591, 1920.048,1479.247, 996.877, 747.490, 454.921, 231.373,
     5 2515.138, 2259.153,1724.339,1141.683, 842.962, 498.602, 244.925,
     6 2920.507, 2615.817,1980.439,1290.955, 940.092, 541.878, 257.878,
     7 3340.336, 2984.477,2243.638,1442.538,1037.576, 584.289, 270.167,
     8 3768.277, 3359.616,2510.115,1594.395,1134.221, 625.449, 281.750,
     9 4198.305, 3736.013,2776.304,1744.674,1228.977, 665.042, 292.603/
      data pf442/
     1 4624.931, 4108.937,3039.010,1891.763,1320.956, 702.826, 302.718,
     2 5043.356, 4474.264,3295.479,2034.311,1409.440, 738.624, 312.101,
     3 5449.547, 4828.543,3543.436,2171.235,1493.876, 772.324, 320.766,
     4 5840.258, 5169.008,3781.086,2301.713,1573.868, 803.865, 328.740,
     5 6213.005, 5493.558,4007.089,2425.162,1649.158, 833.233, 336.051,
     6 6566.016, 5800.704,4220.523,2541.218,1719.610, 860.449, 342.734,
     7 6898.153, 6089.505,4420.834,2649.700,1785.194, 885.567, 348.827,
     8   16.529,   16.529,  16.529,  16.529,  16.529,  16.529,  16.529,
     9   17.012,   17.012,  17.012,  17.012,  17.012,  17.012,  17.012/
      data pf443/
     1   17.518,   17.518,  17.518,  17.518,  17.518,  17.518,  17.518,
     2   18.047,   18.047,  18.047,  18.047,  18.047,  18.047,  18.047,
     3   18.599,   18.599,  18.599,  18.599,  18.599,  18.599,  18.599,
     4   19.175,   19.175,  19.175,  19.175,  19.175,  19.175,  19.175,
     5   19.774,   19.774,  19.774,  19.774,  19.774,  19.774,  19.774,
     6   20.398,   20.398,  20.398,  20.398,  20.398,  20.398,  20.398,
     7   21.046,   21.046,  21.046,  21.046,  21.046,  21.046,  21.046,
     8   21.719,   21.719,  21.719,  21.719,  21.719,  21.719,  21.719,
     9   22.417,   22.417,  22.417,  22.417,  22.417,  22.417,  22.417/
      data pf444/
     1   23.140,   23.140,  23.140,  23.140,  23.140,  23.140,  23.140,
     2   23.890,   23.890,  23.890,  23.890,  23.890,  23.890,  23.890,
     3   24.666,   24.666,  24.666,  24.666,  24.666,  24.666,  24.666,
     4   25.470,   25.470,  25.470,  25.470,  25.470,  25.470,  25.470,
     5   26.302,   26.302,  26.302,  26.302,  26.302,  26.302,  26.302,
     6   27.163,   27.163,  27.163,  27.163,  27.163,  27.163,  27.163,
     7   28.055,   28.055,  28.055,  28.055,  28.055,  28.055,  28.055,
     8   28.980,   28.980,  28.980,  28.980,  28.980,  28.980,  28.980,
     9   29.938,   29.938,  29.938,  29.938,  29.938,  29.938,  29.938/
      data pf445/
     1   31.442,   31.442,  31.442,  31.442,  31.442,  31.442,  31.442,
     2   33.035,   33.035,  33.035,  33.035,  33.035,  33.035,  33.035,
     3   34.726,   34.726,  34.726,  34.726,  34.726,  34.726,  34.726,
     4   36.525,   36.525,  36.525,  36.525,  36.525,  36.525,  36.525,
     5   38.446,   38.446,  38.446,  38.446,  38.446,  38.446,  38.446,
     6   40.502,   40.502,  40.502,  40.502,  40.502,  40.502,  40.502,
     7   42.710,   42.710,  42.710,  42.710,  42.710,  42.710,  42.710,
     8   45.091,   45.091,  45.091,  45.091,  45.091,  45.091,  45.091,
     9   47.665,   47.665,  47.665,  47.665,  47.665,  47.665,  47.665/
      data pf446/
     1   50.459,   50.459,  50.459,  50.459,  50.459,  50.459,  50.459,
     2   55.690,   55.690,  55.690,  55.690,  55.690,  55.689,  55.687,
     3   61.801,   61.801,  61.801,  61.801,  61.800,  61.797,  61.788,
     4   69.059,   69.059,  69.058,  69.057,  69.050,  69.036,  69.000,
     5   77.874,   77.873,  77.870,  77.866,  77.834,  77.779,  77.653,
     6   88.917,   88.910,  88.901,  88.884,  88.767,  88.577,  88.191,
     7  103.302,  103.280, 103.251, 103.195, 102.823, 102.243, 101.201,
     8  122.872,  122.807, 122.722, 122.559, 121.516, 119.950, 117.419,
     9  150.532,  150.362, 150.142, 149.725, 147.107, 143.313, 137.720/
      data pf447/
     1  190.603,  190.204, 189.690, 188.723, 182.779, 174.429, 163.082,
     2  249.049,  248.195, 247.100, 245.055, 232.710, 215.841, 194.510,
     3  333.482,  331.801, 329.651, 325.666, 301.984, 270.411, 232.947,
     4  452.844,  449.767, 445.846, 438.623, 396.300, 341.095, 279.177,
     5  616.752,  611.480, 604.779, 592.508, 521.497, 430.657, 333.727,
     6  834.601,  826.081, 815.279, 795.598, 682.972, 541.368, 396.801,
     7 1114.555, 1101.488,1084.955,1054.971, 885.080, 674.737, 468.247,
     8 1462.625, 1443.495,1419.335,1375.699,1130.626, 831.330, 547.551,
     9 1881.966, 1855.096,1821.218,1760.252,1420.534,1010.680, 633.882/
      data pf448/
     1 2372.506, 2336.133,2290.344,2208.208,1753.721,1211.323, 726.144,
     2 2930.938, 2883.298,2823.402,2716.274,2127.186,1430.914, 823.062,
     3 3551.027, 3490.434,3414.342,3278.596,2536.280,1666.416, 923.259,
     4 4224.162, 4149.082,4054.899,3887.262,2975.083,1914.330,1025.338,
     5 4940.042, 4849.157,4735.253,4532.930,3436.855,2170.923,1127.946,
     6 5687.411, 5579.654,5444.719,5205.476,3914.474,2432.440,1229.835,
     7 6454.747, 6329.332,6172.401,5894.611,4400.839,2695.291,1329.894,
     8 7230.861, 7087.282,6907.743,6590.391,4889.205,2956.181,1427.175,
     9 8005.359, 7843.387,7640.966,7283.632,5373.430,3212.212,1520.902/
      data pf449/
     1   17.797,   17.797,  17.797,  17.797,  17.797,  17.797,  17.797,
     2   18.083,   18.083,  18.083,  18.083,  18.083,  18.083,  18.083,
     3   18.368,   18.368,  18.368,  18.368,  18.368,  18.368,  18.368,
     4   18.650,   18.650,  18.650,  18.650,  18.650,  18.650,  18.650,
     5   18.930,   18.930,  18.930,  18.930,  18.930,  18.930,  18.930,
     6   19.207,   19.207,  19.207,  19.207,  19.207,  19.207,  19.207,
     7   19.481,   19.481,  19.481,  19.481,  19.481,  19.481,  19.481,
     8   19.752,   19.752,  19.752,  19.752,  19.752,  19.752,  19.752,
     9   20.021,   20.021,  20.021,  20.021,  20.021,  20.021,  20.021/
      data pf450/
     1   20.286,   20.286,  20.286,  20.286,  20.286,  20.286,  20.286,
     2   20.549,   20.549,  20.549,  20.549,  20.549,  20.549,  20.549,
     3   20.809,   20.809,  20.809,  20.809,  20.809,  20.809,  20.809,
     4   21.068,   21.068,  21.068,  21.068,  21.068,  21.068,  21.068,
     5   21.326,   21.326,  21.326,  21.326,  21.326,  21.326,  21.326,
     6   21.583,   21.583,  21.583,  21.583,  21.583,  21.583,  21.583,
     7   21.842,   21.842,  21.842,  21.842,  21.842,  21.842,  21.842,
     8   22.102,   22.102,  22.102,  22.102,  22.102,  22.102,  22.102,
     9   22.366,   22.366,  22.366,  22.366,  22.366,  22.366,  22.366/
      data pf451/
     1   22.634,   22.634,  22.634,  22.634,  22.634,  22.634,  22.634,
     2   22.909,   22.909,  22.909,  22.909,  22.909,  22.909,  22.909,
     3   23.339,   23.339,  23.339,  23.339,  23.339,  23.339,  23.339,
     4   23.795,   23.795,  23.795,  23.795,  23.795,  23.795,  23.795,
     5   24.285,   24.285,  24.285,  24.285,  24.285,  24.285,  24.285,
     6   24.817,   24.817,  24.817,  24.817,  24.817,  24.817,  24.817,
     7   25.401,   25.401,  25.401,  25.401,  25.401,  25.401,  25.401,
     8   26.044,   26.044,  26.044,  26.044,  26.044,  26.044,  26.044,
     9   26.756,   26.756,  26.756,  26.756,  26.756,  26.756,  26.756/
      data pf452/
     1   27.546,   27.546,  27.546,  27.546,  27.546,  27.546,  27.546,
     2   28.421,   28.421,  28.421,  28.421,  28.421,  28.421,  28.421,
     3   29.390,   29.390,  29.390,  29.390,  29.390,  29.390,  29.390,
     4   31.236,   31.236,  31.236,  31.236,  31.236,  31.236,  31.236,
     5   33.407,   33.407,  33.407,  33.407,  33.407,  33.407,  33.407,
     6   35.948,   35.948,  35.948,  35.948,  35.948,  35.948,  35.948,
     7   38.921,   38.921,  38.921,  38.921,  38.921,  38.921,  38.921,
     8   42.415,   42.415,  42.415,  42.415,  42.415,  42.415,  42.415,
     9   46.561,   46.561,  46.561,  46.561,  46.561,  46.561,  46.560/
      data pf453/
     1   51.557,   51.557,  51.557,  51.556,  51.556,  51.556,  51.555,
     2   57.701,   57.701,  57.700,  57.699,  57.699,  57.696,  57.691,
     3   65.452,   65.452,  65.450,  65.445,  65.444,  65.429,  65.406,
     4   75.543,   75.541,  75.533,  75.512,  75.506,  75.445,  75.351,
     5   89.176,   89.171,  89.139,  89.060,  89.038,  88.820,  88.504,
     6  108.367,  108.353, 108.247, 107.992, 107.918, 107.241, 106.307,
     7  136.433,  136.392, 136.087, 135.358, 135.151, 133.290, 130.833,
     8  178.598,  178.491, 177.710, 175.850, 175.329, 170.749, 164.928,
     9  242.584,  242.337, 240.530, 236.249, 235.060, 224.835, 212.281/
      data pf454/
     1  339.003,  338.480, 334.662, 325.661, 323.183, 302.265, 277.352,
     2  481.306,  480.286, 472.853, 455.397, 450.628, 411.038, 365.145,
     3  685.190,  683.339, 669.880, 638.377, 629.830, 559.923, 480.809,
     4  967.419,  964.272, 941.422, 888.106, 873.729, 757.688, 629.139,
     5 1344.232, 1339.181,1302.561,1217.344,1194.491,1012.195, 814.050,
     6 1829.603, 1821.902,1766.146,1636.714,1602.174,1329.519,1038.122,
     7 2433.646, 2422.432,2341.335,2153.481,2103.569,1713.232,1302.284,
     8 3161.450, 3145.774,3032.527,2770.706,2701.411,2163.980,1605.683,
     9 4012.485, 3991.357,3838.855,3486.883,3394.048,2679.379,1945.741/
      data pf455/
     1 4980.635, 4953.066,4754.242,4296.060,4175.583,3254.227,2318.375,
     2 6054.767, 6019.822,5767.974,5188.396,5036.416,3880.956,2718.328,
     3   17.424,   17.424,  17.424,  17.424,  17.424,  17.424,  17.424,
     4   17.662,   17.662,  17.662,  17.662,  17.662,  17.662,  17.662,
     5   17.897,   17.897,  17.897,  17.897,  17.897,  17.897,  17.897,
     6   18.128,   18.128,  18.128,  18.128,  18.128,  18.128,  18.128,
     7   18.355,   18.355,  18.355,  18.355,  18.355,  18.355,  18.355,
     8   18.577,   18.577,  18.577,  18.577,  18.577,  18.577,  18.577,
     9   18.795,   18.795,  18.795,  18.795,  18.795,  18.795,  18.795/
      data pf456/
     1   19.009,   19.009,  19.009,  19.009,  19.009,  19.009,  19.009,
     2   19.218,   19.218,  19.218,  19.218,  19.218,  19.218,  19.218,
     3   19.422,   19.422,  19.422,  19.422,  19.422,  19.422,  19.422,
     4   19.621,   19.621,  19.621,  19.621,  19.621,  19.621,  19.621,
     5   19.815,   19.815,  19.815,  19.815,  19.815,  19.815,  19.815,
     6   20.006,   20.006,  20.006,  20.006,  20.006,  20.006,  20.006,
     7   20.191,   20.191,  20.191,  20.191,  20.191,  20.191,  20.191,
     8   20.373,   20.373,  20.373,  20.373,  20.373,  20.373,  20.373,
     9   20.552,   20.552,  20.552,  20.552,  20.552,  20.552,  20.552/
      data pf457/
     1   20.727,   20.727,  20.727,  20.727,  20.727,  20.727,  20.727,
     2   20.900,   20.900,  20.900,  20.900,  20.900,  20.900,  20.900,
     3   21.072,   21.072,  21.072,  21.072,  21.072,  21.072,  21.072,
     4   21.244,   21.244,  21.244,  21.244,  21.244,  21.244,  21.244,
     5   21.504,   21.504,  21.504,  21.504,  21.504,  21.504,  21.504,
     6   21.773,   21.773,  21.773,  21.773,  21.773,  21.773,  21.773,
     7   22.058,   22.058,  22.058,  22.058,  22.058,  22.058,  22.058,
     8   22.368,   22.368,  22.368,  22.368,  22.368,  22.368,  22.368,
     9   22.713,   22.713,  22.713,  22.713,  22.713,  22.713,  22.713/
      data pf458/
     1   23.105,   23.105,  23.105,  23.105,  23.105,  23.105,  23.105,
     2   23.558,   23.558,  23.558,  23.558,  23.558,  23.558,  23.558,
     3   24.086,   24.086,  24.086,  24.086,  24.086,  24.086,  24.086,
     4   24.705,   24.705,  24.705,  24.705,  24.705,  24.705,  24.705,
     5   25.430,   25.430,  25.430,  25.430,  25.430,  25.430,  25.430,
     6   26.918,   26.918,  26.918,  26.918,  26.918,  26.918,  26.918,
     7   28.818,   28.818,  28.818,  28.818,  28.818,  28.818,  28.818,
     8   31.194,   31.194,  31.194,  31.194,  31.194,  31.194,  31.194,
     9   34.098,   34.098,  34.098,  34.098,  34.098,  34.098,  34.098/
      data pf459/
     1   37.567,   37.567,  37.567,  37.567,  37.567,  37.567,  37.567,
     2   41.625,   41.625,  41.625,  41.625,  41.625,  41.625,  41.625,
     3   46.284,   46.284,  46.284,  46.284,  46.284,  46.284,  46.284,
     4   51.552,   51.552,  51.552,  51.552,  51.552,  51.552,  51.552,
     5   57.449,   57.449,  57.449,  57.449,  57.449,  57.449,  57.449,
     6   64.032,   64.032,  64.032,  64.032,  64.032,  64.032,  64.032,
     7   71.428,   71.428,  71.428,  71.428,  71.428,  71.427,  71.427,
     8   79.892,   79.892,  79.891,  79.891,  79.890,  79.887,  79.883,
     9   89.888,   89.888,  89.887,  89.884,  89.879,  89.866,  89.847/
      data pf460/
     1  102.237,  102.235, 102.230, 102.217, 102.196, 102.144, 102.070,
     2  118.341,  118.335, 118.316, 118.269, 118.194, 118.012, 117.764,
     3  140.547,  140.529, 140.469, 140.323, 140.089, 139.532, 138.801,
     4  172.630,  172.579, 172.413, 172.007, 171.365, 169.857, 167.944,
     5  220.347,  220.221, 219.808, 218.803, 217.222, 213.553, 209.047,
     6  291.930,  291.647, 290.716, 288.458, 284.932, 276.830, 267.157,
     7  398.315,  397.730, 395.812, 391.168, 383.959, 367.544, 348.433,
     8  552.936,  551.819, 548.166, 539.335, 525.699, 494.899, 459.835,
     9  770.983,  768.998, 762.510, 746.852, 722.784, 668.816, 608.588/
      data pf461/
     1 1068.169, 1064.853,1054.026,1027.940, 988.005, 899.037, 801.493,
     2 1459.168, 1453.931,1436.844,1395.729,1333.019,1194.112,1044.197,
     3 1956.016, 1948.144,1922.483,1860.811,1767.051,1560.429,1340.543,
     4 2566.728, 2555.409,2518.538,2430.020,2295.835,2001.477,1692.115,
     5    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     8    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     9    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000/
      data pf462/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     5    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     8    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     9    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000/
      data pf463/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     5    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     6    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     7    6.002,    6.002,   6.002,   6.002,   6.002,   6.002,   6.002,
     8    6.004,    6.004,   6.004,   6.004,   6.004,   6.004,   6.004,
     9    6.008,    6.008,   6.008,   6.008,   6.008,   6.008,   6.008/
      data pf464/
     1    6.014,    6.014,   6.014,   6.014,   6.014,   6.014,   6.014,
     2    6.024,    6.024,   6.024,   6.024,   6.024,   6.024,   6.024,
     3    6.042,    6.042,   6.042,   6.042,   6.042,   6.042,   6.042,
     4    6.068,    6.068,   6.068,   6.068,   6.068,   6.068,   6.068,
     5    6.109,    6.109,   6.109,   6.109,   6.109,   6.109,   6.109,
     6    6.170,    6.170,   6.170,   6.170,   6.170,   6.170,   6.170,
     7    6.258,    6.258,   6.258,   6.258,   6.258,   6.258,   6.258,
     8    6.489,    6.489,   6.489,   6.489,   6.489,   6.489,   6.489,
     9    6.875,    6.875,   6.875,   6.875,   6.875,   6.875,   6.875/
      data pf465/
     1    7.485,    7.485,   7.485,   7.485,   7.485,   7.485,   7.485,
     2    8.405,    8.405,   8.405,   8.405,   8.405,   8.405,   8.405,
     3    9.729,    9.729,   9.729,   9.729,   9.729,   9.729,   9.729,
     4   11.560,   11.560,  11.560,  11.560,  11.560,  11.560,  11.560,
     5   14.000,   14.000,  14.000,  14.000,  14.000,  14.000,  14.000,
     6   17.141,   17.141,  17.141,  17.141,  17.141,  17.141,  17.141,
     7   21.059,   21.059,  21.059,  21.059,  21.059,  21.059,  21.059,
     8   25.807,   25.807,  25.807,  25.807,  25.807,  25.807,  25.807,
     9   31.415,   31.415,  31.415,  31.415,  31.415,  31.415,  31.415/
      data pf466/
     1   37.894,   37.894,  37.894,  37.894,  37.894,  37.894,  37.894,
     2   45.250,   45.250,  45.250,  45.250,  45.250,  45.250,  45.250,
     3   53.515,   53.515,  53.515,  53.515,  53.515,  53.514,  53.514,
     4   62.795,   62.795,  62.795,  62.795,  62.794,  62.793,  62.791,
     5   73.365,   73.365,  73.365,  73.363,  73.358,  73.350,  73.341,
     6   85.838,   85.837,  85.837,  85.828,  85.805,  85.765,  85.724,
     7  101.506,  101.502, 101.499, 101.465, 101.370, 101.210, 101.052,
     8  122.955,  122.940, 122.933, 122.812, 122.479, 121.928, 121.397,
     9  155.081,  155.035, 155.011, 154.643, 153.624, 151.963, 150.400/
      data pf467/
     1  206.521,  206.396, 206.331, 205.331, 202.580, 198.139, 194.041,
     2  291.354,  291.048, 290.890, 288.456, 281.782, 271.115, 261.441,
     3  430.654,  429.976, 429.626, 424.248, 409.545, 386.249, 365.452,
     4  653.340,  651.960, 651.251, 640.348, 610.626, 563.892, 522.750,
     5  995.774,  993.177, 991.843, 971.378, 915.720, 828.803, 753.232,
     6 1499.805, 1495.243,1492.901,1457.028,1359.676,1208.574,1078.643,
     7   12.805,   12.805,  12.805,  12.805,  12.805,  12.805,  12.805,
     8   13.147,   13.147,  13.147,  13.147,  13.147,  13.147,  13.147,
     9   13.487,   13.487,  13.487,  13.487,  13.487,  13.487,  13.487/
      data pf468/
     1   13.823,   13.823,  13.823,  13.823,  13.823,  13.823,  13.823,
     2   14.157,   14.157,  14.157,  14.157,  14.157,  14.157,  14.157,
     3   14.486,   14.486,  14.486,  14.486,  14.486,  14.486,  14.486,
     4   14.811,   14.811,  14.811,  14.811,  14.811,  14.811,  14.811,
     5   15.131,   15.131,  15.131,  15.131,  15.131,  15.131,  15.131,
     6   15.446,   15.446,  15.446,  15.446,  15.446,  15.446,  15.446,
     7   15.756,   15.756,  15.756,  15.756,  15.756,  15.756,  15.756,
     8   16.061,   16.061,  16.061,  16.061,  16.061,  16.061,  16.061,
     9   16.359,   16.359,  16.359,  16.359,  16.359,  16.359,  16.359/
      data pf469/
     1   16.652,   16.652,  16.652,  16.652,  16.652,  16.652,  16.652,
     2   16.939,   16.939,  16.939,  16.939,  16.939,  16.939,  16.939,
     3   17.219,   17.219,  17.219,  17.219,  17.219,  17.219,  17.219,
     4   17.493,   17.493,  17.493,  17.493,  17.493,  17.493,  17.493,
     5   17.761,   17.761,  17.761,  17.761,  17.761,  17.761,  17.761,
     6   18.024,   18.024,  18.024,  18.024,  18.024,  18.024,  18.024,
     7   18.280,   18.280,  18.280,  18.280,  18.280,  18.280,  18.280,
     8   18.532,   18.532,  18.532,  18.532,  18.532,  18.532,  18.532,
     9   18.900,   18.900,  18.900,  18.900,  18.900,  18.900,  18.900/
      data pf470/
     1   19.261,   19.261,  19.261,  19.261,  19.261,  19.261,  19.261,
     2   19.617,   19.617,  19.617,  19.617,  19.617,  19.617,  19.617,
     3   19.975,   19.975,  19.975,  19.975,  19.975,  19.975,  19.975,
     4   20.340,   20.340,  20.340,  20.340,  20.340,  20.340,  20.340,
     5   20.720,   20.720,  20.720,  20.720,  20.720,  20.720,  20.720,
     6   21.125,   21.125,  21.125,  21.125,  21.125,  21.125,  21.125,
     7   21.567,   21.567,  21.567,  21.567,  21.567,  21.567,  21.567,
     8   22.058,   22.058,  22.058,  22.058,  22.058,  22.058,  22.058,
     9   22.612,   22.612,  22.612,  22.612,  22.612,  22.612,  22.612/
      data pf471/
     1   23.720,   23.720,  23.720,  23.720,  23.720,  23.720,  23.720,
     2   25.123,   25.123,  25.123,  25.123,  25.123,  25.123,  25.123,
     3   26.896,   26.896,  26.896,  26.896,  26.896,  26.896,  26.896,
     4   29.109,   29.109,  29.109,  29.109,  29.109,  29.109,  29.109,
     5   31.820,   31.820,  31.820,  31.820,  31.820,  31.820,  31.820,
     6   35.075,   35.075,  35.075,  35.075,  35.075,  35.075,  35.075,
     7   38.900,   38.900,  38.900,  38.900,  38.900,  38.900,  38.900,
     8   43.302,   43.302,  43.302,  43.302,  43.302,  43.302,  43.302,
     9   48.268,   48.268,  48.268,  48.268,  48.268,  48.268,  48.268/
      data pf472/
     1   53.768,   53.768,  53.768,  53.768,  53.768,  53.768,  53.768,
     2   59.753,   59.753,  59.753,  59.753,  59.753,  59.753,  59.753,
     3   66.163,   66.163,  66.163,  66.163,  66.163,  66.163,  66.163,
     4   72.930,   72.930,  72.930,  72.930,  72.930,  72.930,  72.930,
     5   79.989,   79.989,  79.989,  79.989,  79.989,  79.989,  79.989,
     6   87.289,   87.289,  87.289,  87.289,  87.289,  87.289,  87.289,
     7   94.819,   94.819,  94.819,  94.819,  94.819,  94.819,  94.818,
     8  102.638,  102.638, 102.638, 102.638, 102.638, 102.637, 102.636,
     9  110.943,  110.943, 110.943, 110.943, 110.942, 110.940, 110.934/
      data pf473/
     1  120.188,  120.188, 120.188, 120.188, 120.184, 120.172, 120.144,
     2  131.315,  131.315, 131.315, 131.315, 131.301, 131.251, 131.135,
     3  146.202,  146.202, 146.201, 146.201, 146.152, 145.972, 145.568,
     4  168.391,  168.390, 168.388, 168.387, 168.232, 167.676, 166.442,
     5  204.150,  204.147, 204.141, 204.139, 203.711, 202.188, 198.855,
     6  263.755,  263.747, 263.733, 263.729, 262.669, 258.930, 250.850,
     7  362.699,  362.681, 362.648, 362.639, 360.266, 351.940, 334.145,
     8  522.406,  522.369, 522.302, 522.283, 517.411, 500.416, 464.455,
     9   11.863,   11.863,  11.863,  11.863,  11.863,  11.863,  11.863/
      data pf474/
     1   12.214,   12.214,  12.214,  12.214,  12.214,  12.214,  12.214,
     2   12.569,   12.569,  12.569,  12.569,  12.569,  12.569,  12.569,
     3   12.927,   12.927,  12.927,  12.927,  12.927,  12.927,  12.927,
     4   13.287,   13.287,  13.287,  13.287,  13.287,  13.287,  13.287,
     5   13.648,   13.648,  13.648,  13.648,  13.648,  13.648,  13.648,
     6   14.010,   14.010,  14.010,  14.010,  14.010,  14.010,  14.010,
     7   14.372,   14.372,  14.372,  14.372,  14.372,  14.372,  14.372,
     8   14.734,   14.734,  14.734,  14.734,  14.734,  14.734,  14.734,
     9   15.094,   15.094,  15.094,  15.094,  15.094,  15.094,  15.094/
      data pf475/
     1   15.453,   15.453,  15.453,  15.453,  15.453,  15.453,  15.453,
     2   15.810,   15.810,  15.810,  15.810,  15.810,  15.810,  15.810,
     3   16.165,   16.165,  16.165,  16.165,  16.165,  16.165,  16.165,
     4   16.516,   16.516,  16.516,  16.516,  16.516,  16.516,  16.516,
     5   16.865,   16.865,  16.865,  16.865,  16.865,  16.865,  16.865,
     6   17.210,   17.210,  17.210,  17.210,  17.210,  17.210,  17.210,
     7   17.553,   17.553,  17.553,  17.553,  17.553,  17.553,  17.553,
     8   17.892,   17.892,  17.892,  17.892,  17.892,  17.892,  17.892,
     9   18.228,   18.228,  18.228,  18.228,  18.228,  18.228,  18.228/
      data pf476/
     1   18.561,   18.561,  18.561,  18.561,  18.561,  18.561,  18.561,
     2   19.058,   19.058,  19.058,  19.058,  19.058,  19.058,  19.058,
     3   19.553,   19.553,  19.553,  19.553,  19.553,  19.553,  19.553,
     4   20.049,   20.049,  20.049,  20.049,  20.049,  20.049,  20.049,
     5   20.550,   20.550,  20.550,  20.550,  20.550,  20.550,  20.550,
     6   21.063,   21.063,  21.063,  21.063,  21.063,  21.063,  21.063,
     7   21.592,   21.592,  21.592,  21.592,  21.592,  21.592,  21.592,
     8   22.146,   22.146,  22.146,  22.146,  22.146,  22.146,  22.146,
     9   22.731,   22.731,  22.731,  22.731,  22.731,  22.731,  22.731/
      data pf477/
     1   23.356,   23.356,  23.356,  23.356,  23.356,  23.356,  23.356,
     2   24.029,   24.029,  24.029,  24.029,  24.029,  24.029,  24.029,
     3   25.279,   25.279,  25.279,  25.279,  25.279,  25.279,  25.279,
     4   26.721,   26.721,  26.721,  26.721,  26.721,  26.721,  26.721,
     5   28.386,   28.386,  28.386,  28.386,  28.386,  28.386,  28.386,
     6   30.297,   30.297,  30.297,  30.297,  30.297,  30.297,  30.297,
     7   32.469,   32.469,  32.469,  32.469,  32.469,  32.469,  32.469,
     8   34.904,   34.904,  34.904,  34.904,  34.904,  34.904,  34.904,
     9   37.597,   37.597,  37.597,  37.597,  37.597,  37.597,  37.597/
      data pf478/
     1   40.532,   40.532,  40.532,  40.532,  40.532,  40.532,  40.532,
     2   43.686,   43.686,  43.686,  43.686,  43.686,  43.686,  43.686,
     3   47.028,   47.028,  47.028,  47.028,  47.028,  47.028,  47.028,
     4   50.522,   50.522,  50.522,  50.522,  50.522,  50.522,  50.522,
     5   54.131,   54.131,  54.131,  54.131,  54.131,  54.131,  54.131,
     6   57.814,   57.814,  57.814,  57.814,  57.814,  57.814,  57.814,
     7   61.534,   61.534,  61.534,  61.534,  61.534,  61.534,  61.534,
     8   65.255,   65.255,  65.255,  65.255,  65.255,  65.255,  65.255,
     9   68.953,   68.953,  68.953,  68.953,  68.953,  68.953,  68.953/
      data pf479/
     1   72.624,   72.624,  72.624,  72.624,  72.624,  72.624,  72.624,
     2   76.305,   76.305,  76.305,  76.305,  76.305,  76.305,  76.305,
     3   80.103,   80.103,  80.103,  80.103,  80.103,  80.103,  80.103,
     4   84.254,   84.254,  84.254,  84.254,  84.254,  84.254,  84.254,
     5   89.213,   89.213,  89.213,  89.213,  89.212,  89.212,  89.211,
     6   95.801,   95.801,  95.801,  95.801,  95.800,  95.799,  95.796,
     7  105.459,  105.459, 105.458, 105.458, 105.455, 105.449, 105.439,
     8  120.611,  120.611, 120.609, 120.608, 120.597, 120.578, 120.549,
     9  145.168,  145.168, 145.161, 145.158, 145.128, 145.076, 144.996/
      data pf480/
     1  185.079,  185.079, 185.061, 185.054, 184.979, 184.851, 184.655,
     2    8.647,    8.647,   8.647,   8.647,   8.647,   8.647,   8.647,
     3    8.872,    8.872,   8.872,   8.872,   8.872,   8.872,   8.872,
     4    9.101,    9.101,   9.101,   9.101,   9.101,   9.101,   9.101,
     5    9.334,    9.334,   9.334,   9.334,   9.334,   9.334,   9.334,
     6    9.572,    9.572,   9.572,   9.572,   9.572,   9.572,   9.572,
     7    9.814,    9.814,   9.814,   9.814,   9.814,   9.814,   9.814,
     8   10.058,   10.058,  10.058,  10.058,  10.058,  10.058,  10.058,
     9   10.305,   10.305,  10.305,  10.305,  10.305,  10.305,  10.305/
      data pf481/
     1   10.555,   10.555,  10.555,  10.555,  10.555,  10.555,  10.555,
     2   10.806,   10.806,  10.806,  10.806,  10.806,  10.806,  10.806,
     3   11.058,   11.058,  11.058,  11.058,  11.058,  11.058,  11.058,
     4   11.311,   11.311,  11.311,  11.311,  11.311,  11.311,  11.311,
     5   11.564,   11.564,  11.564,  11.564,  11.564,  11.564,  11.564,
     6   11.817,   11.817,  11.817,  11.817,  11.817,  11.817,  11.817,
     7   12.070,   12.070,  12.070,  12.070,  12.070,  12.070,  12.070,
     8   12.323,   12.323,  12.323,  12.323,  12.323,  12.323,  12.323,
     9   12.574,   12.574,  12.574,  12.574,  12.574,  12.574,  12.574/
      data pf482/
     1   12.825,   12.825,  12.825,  12.825,  12.825,  12.825,  12.825,
     2   13.074,   13.074,  13.074,  13.074,  13.074,  13.074,  13.074,
     3   13.323,   13.323,  13.323,  13.323,  13.323,  13.323,  13.323,
     4   13.693,   13.693,  13.693,  13.693,  13.693,  13.693,  13.693,
     5   14.062,   14.062,  14.062,  14.062,  14.062,  14.062,  14.062,
     6   14.430,   14.430,  14.430,  14.430,  14.430,  14.430,  14.430,
     7   14.798,   14.798,  14.798,  14.798,  14.798,  14.798,  14.798,
     8   15.168,   15.168,  15.168,  15.168,  15.168,  15.168,  15.168,
     9   15.541,   15.541,  15.541,  15.541,  15.541,  15.541,  15.541/
      data pf483/
     1   15.920,   15.920,  15.920,  15.920,  15.920,  15.920,  15.920,
     2   16.308,   16.308,  16.308,  16.308,  16.308,  16.308,  16.308,
     3   16.705,   16.705,  16.705,  16.705,  16.705,  16.705,  16.705,
     4   17.116,   17.116,  17.116,  17.116,  17.116,  17.116,  17.116,
     5   17.833,   17.833,  17.833,  17.833,  17.833,  17.833,  17.833,
     6   18.600,   18.600,  18.600,  18.600,  18.600,  18.600,  18.600,
     7   19.424,   19.424,  19.424,  19.424,  19.424,  19.424,  19.424,
     8   20.306,   20.306,  20.306,  20.306,  20.306,  20.306,  20.306,
     9   21.246,   21.246,  21.246,  21.246,  21.246,  21.246,  21.246/
      data pf484/
     1   22.241,   22.241,  22.241,  22.241,  22.241,  22.241,  22.241,
     2   23.284,   23.284,  23.284,  23.284,  23.284,  23.284,  23.284,
     3   24.366,   24.366,  24.366,  24.366,  24.366,  24.366,  24.366,
     4   25.476,   25.476,  25.476,  25.476,  25.476,  25.476,  25.476,
     5   26.603,   26.603,  26.603,  26.603,  26.603,  26.603,  26.603,
     6   27.735,   27.735,  27.735,  27.735,  27.735,  27.735,  27.735,
     7   28.861,   28.861,  28.861,  28.861,  28.861,  28.861,  28.861,
     8   29.970,   29.970,  29.970,  29.970,  29.970,  29.970,  29.970,
     9   31.053,   31.053,  31.053,  31.053,  31.053,  31.053,  31.053/
      data pf485/
     1   32.104,   32.104,  32.104,  32.104,  32.104,  32.104,  32.104,
     2   33.121,   33.121,  33.121,  33.121,  33.121,  33.121,  33.121,
     3   34.110,   34.110,  34.110,  34.110,  34.110,  34.110,  34.110,
     4   35.096,   35.096,  35.096,  35.096,  35.096,  35.096,  35.096,
     5   36.132,   36.132,  36.132,  36.132,  36.132,  36.132,  36.132,
     6   37.320,   37.320,  37.320,  37.320,  37.320,  37.320,  37.320,
     7   38.833,   38.833,  38.833,  38.833,  38.833,  38.833,  38.833,
     8   40.940,   40.940,  40.940,  40.940,  40.940,  40.940,  40.940,
     9   44.038,   44.038,  44.038,  44.038,  44.038,  44.038,  44.038/
      data pf486/
     1   48.700,   48.700,  48.700,  48.700,  48.700,  48.700,  48.700,
     2   55.748,   55.748,  55.748,  55.748,  55.748,  55.748,  55.748,
     3   66.359,   66.359,  66.359,  66.359,  66.359,  66.359,  66.359,
     4    5.109,    5.109,   5.109,   5.109,   5.109,   5.109,   5.109,
     5    5.197,    5.197,   5.197,   5.197,   5.197,   5.197,   5.197,
     6    5.287,    5.287,   5.287,   5.287,   5.287,   5.287,   5.287,
     7    5.379,    5.379,   5.379,   5.379,   5.379,   5.379,   5.379,
     8    5.474,    5.474,   5.474,   5.474,   5.474,   5.474,   5.474,
     9    5.570,    5.570,   5.570,   5.570,   5.570,   5.570,   5.570/
      data pf487/
     1    5.668,    5.668,   5.668,   5.668,   5.668,   5.668,   5.668,
     2    5.767,    5.767,   5.767,   5.767,   5.767,   5.767,   5.767,
     3    5.866,    5.866,   5.866,   5.866,   5.866,   5.866,   5.866,
     4    5.967,    5.967,   5.967,   5.967,   5.967,   5.967,   5.967,
     5    6.068,    6.068,   6.068,   6.068,   6.068,   6.068,   6.068,
     6    6.170,    6.170,   6.170,   6.170,   6.170,   6.170,   6.170,
     7    6.272,    6.272,   6.272,   6.272,   6.272,   6.272,   6.272,
     8    6.373,    6.373,   6.373,   6.373,   6.373,   6.373,   6.373,
     9    6.474,    6.474,   6.474,   6.474,   6.474,   6.474,   6.474/
      data pf488/
     1    6.575,    6.575,   6.575,   6.575,   6.575,   6.575,   6.575,
     2    6.675,    6.675,   6.675,   6.675,   6.675,   6.675,   6.675,
     3    6.774,    6.774,   6.774,   6.774,   6.774,   6.774,   6.774,
     4    6.872,    6.872,   6.872,   6.872,   6.872,   6.872,   6.872,
     5    6.969,    6.969,   6.969,   6.969,   6.969,   6.969,   6.969,
     6    7.111,    7.111,   7.111,   7.111,   7.111,   7.111,   7.111,
     7    7.251,    7.251,   7.251,   7.251,   7.251,   7.251,   7.251,
     8    7.387,    7.387,   7.387,   7.387,   7.387,   7.387,   7.387,
     9    7.518,    7.518,   7.518,   7.518,   7.518,   7.518,   7.518/
      data pf489/
     1    7.646,    7.646,   7.646,   7.646,   7.646,   7.646,   7.646,
     2    7.769,    7.769,   7.769,   7.769,   7.769,   7.769,   7.769,
     3    7.888,    7.888,   7.888,   7.888,   7.888,   7.888,   7.888,
     4    8.002,    8.002,   8.002,   8.002,   8.002,   8.002,   8.002,
     5    8.112,    8.112,   8.112,   8.112,   8.112,   8.112,   8.112,
     6    8.217,    8.217,   8.217,   8.217,   8.217,   8.217,   8.217,
     7    8.382,    8.382,   8.382,   8.382,   8.382,   8.382,   8.382,
     8    8.534,    8.534,   8.534,   8.534,   8.534,   8.534,   8.534,
     9    8.674,    8.674,   8.674,   8.674,   8.674,   8.674,   8.674/
      data pf490/
     1    8.803,    8.803,   8.803,   8.803,   8.803,   8.803,   8.803,
     2    8.921,    8.921,   8.921,   8.921,   8.921,   8.921,   8.921,
     3    9.028,    9.028,   9.028,   9.028,   9.028,   9.028,   9.028,
     4    9.126,    9.126,   9.126,   9.126,   9.126,   9.126,   9.126,
     5    9.214,    9.214,   9.214,   9.214,   9.214,   9.214,   9.214,
     6    9.294,    9.294,   9.294,   9.294,   9.294,   9.294,   9.294,
     7    9.367,    9.367,   9.367,   9.367,   9.367,   9.367,   9.367,
     8    9.432,    9.432,   9.432,   9.432,   9.432,   9.432,   9.432,
     9    9.491,    9.491,   9.491,   9.491,   9.491,   9.491,   9.491/
      data pf491/
     1    9.545,    9.545,   9.545,   9.545,   9.545,   9.545,   9.545,
     2    9.593,    9.593,   9.593,   9.593,   9.593,   9.593,   9.593,
     3    9.637,    9.637,   9.637,   9.637,   9.637,   9.637,   9.637,
     4    9.679,    9.679,   9.679,   9.679,   9.679,   9.679,   9.679,
     5    9.725,    9.725,   9.725,   9.725,   9.725,   9.725,   9.725,
     6    9.787,    9.787,   9.787,   9.787,   9.787,   9.787,   9.787,
     7    9.886,    9.886,   9.886,   9.886,   9.886,   9.886,   9.886,
     8   10.063,   10.063,  10.063,  10.063,  10.063,  10.063,  10.063,
     9   10.381,   10.381,  10.381,  10.381,  10.381,  10.381,  10.381/
      data pf492/
     1   10.932,   10.932,  10.932,  10.932,  10.932,  10.932,  10.932,
     2   11.841,   11.841,  11.841,  11.841,  11.841,  11.841,  11.841,
     3   13.264,   13.264,  13.264,  13.264,  13.264,  13.264,  13.263,
     4   15.389,   15.388,  15.388,  15.388,  15.388,  15.388,  15.388,
     5   18.444,   18.444,  18.444,  18.444,  18.443,  18.443,  18.442,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf493/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf494/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf495/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf496/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     7    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     8    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     9    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
      data pf497/
     1    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     2    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     3    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     4    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     5    1.000,    1.000,   1.000,   1.000,   1.000,   1.000,   1.000,
     6    1.001,    1.001,   1.001,   1.001,   1.001,   1.001,   1.001,
     7    1.003,    1.003,   1.003,   1.003,   1.003,   1.003,   1.003,
     8    1.009,    1.009,   1.009,   1.009,   1.009,   1.009,   1.009,
     9    1.024,    1.024,   1.024,   1.024,   1.024,   1.024,   1.024/
      data pf498/
     1    1.057,    1.057,   1.057,   1.057,   1.057,   1.057,   1.057,
     2    1.122,    1.122,   1.122,   1.122,   1.122,   1.122,   1.122,
     3    1.245,    1.245,   1.245,   1.245,   1.245,   1.245,   1.245,
     4    1.463,    1.463,   1.463,   1.463,   1.463,   1.463,   1.463,
     5    1.838,    1.838,   1.838,   1.838,   1.838,   1.838,   1.838,
     6    2.462,    2.462,   2.462,   2.462,   2.462,   2.462,   2.462,
     7    3.477,    3.477,   3.477,   3.477,   3.477,   3.477,   3.477,
     8   23.091,   23.091,  23.091,  23.091,  23.091,  23.091,  23.091,
     9   23.504,   23.504,  23.504,  23.504,  23.504,  23.504,  23.504/
      data pf499/
     1   23.919,   23.919,  23.919,  23.919,  23.919,  23.919,  23.919,
     2   24.335,   24.335,  24.335,  24.335,  24.335,  24.335,  24.335,
     3   24.752,   24.752,  24.752,  24.752,  24.752,  24.752,  24.752,
     4   25.168,   25.168,  25.168,  25.168,  25.168,  25.168,  25.168,
     5   25.584,   25.584,  25.584,  25.584,  25.584,  25.584,  25.584,
     6   25.999,   25.999,  25.999,  25.999,  25.999,  25.999,  25.999,
     7   26.413,   26.413,  26.413,  26.413,  26.413,  26.413,  26.413,
     8   26.826,   26.826,  26.826,  26.826,  26.826,  26.826,  26.825,
     9   27.236,   27.236,  27.236,  27.236,  27.236,  27.236,  27.236/
      data pf500/
     1   27.646,   27.646,  27.646,  27.646,  27.646,  27.646,  27.645,
     2   28.053,   28.053,  28.053,  28.053,  28.053,  28.053,  28.053,
     3   28.459,   28.459,  28.459,  28.459,  28.459,  28.459,  28.458,
     4   28.864,   28.864,  28.864,  28.864,  28.864,  28.864,  28.863,
     5   29.269,   29.269,  29.269,  29.269,  29.269,  29.269,  29.267,
     6   29.674,   29.674,  29.674,  29.674,  29.674,  29.674,  29.670,
     7   30.081,   30.081,  30.081,  30.081,  30.081,  30.081,  30.075,
     8   30.491,   30.491,  30.491,  30.491,  30.491,  30.491,  30.481,
     9   30.906,   30.906,  30.906,  30.906,  30.906,  30.906,  30.890/
      data pf501/
     1   31.544,   31.544,  31.543,  31.543,  31.543,  31.542,  31.513,
     2   32.209,   32.209,  32.208,  32.208,  32.207,  32.206,  32.154,
     3   32.917,   32.917,  32.915,  32.914,  32.912,  32.909,  32.820,
     4   33.689,   33.688,  33.684,  33.680,  33.676,  33.669,  33.522,
     5   34.551,   34.550,  34.540,  34.530,  34.521,  34.507,  34.271,
     6   35.543,   35.539,  35.517,  35.496,  35.475,  35.448,  35.080,
     7   36.715,   36.706,  36.660,  36.615,  36.572,  36.523,  35.962,
     8   38.138,   38.121,  38.027,  37.937,  37.854,  37.764,  36.934,
     9   39.907,   39.874,  39.693,  39.519,  39.366,  39.212,  38.009/
      data pf502/
     1   42.152,   42.090,  41.753,  41.436,  41.163,  40.906,  39.203,
     2   47.430,   47.270,  46.408,  45.612,  44.958,  44.400,  41.496,
     3   55.626,   55.252,  53.261,  51.455,  50.025,  48.912,  44.215,
     4   68.318,   67.522,  63.323,  59.572,  56.704,  54.643,  47.401,
     5   87.592,   86.029,  77.865,  70.669,  65.336,  61.766,  51.073,
     6  115.984,  113.135,  98.364,  85.506,  76.232,  70.408,  55.225,
     7  156.323,  151.457, 126.403, 104.834,  89.647,  80.635,  59.832,
     8  211.487,  203.646, 163.525, 129.320, 105.747,  92.448,  64.846,
     9  284.104,  272.109, 211.065, 159.474, 124.591, 105.776,  70.201/
      data pf503/
     1  376.258,  358.738, 270.004, 195.591, 146.122, 120.488,  75.824,
     2  489.252,  464.694, 340.851, 237.710, 170.168, 136.398,  81.633,
     3  623.454,  590.271, 423.582, 285.608, 196.459, 153.282,  87.545,
     4  778.255,  734.863, 517.637, 338.815, 224.642, 170.891,  93.482,
     5  952.132,  897.021, 621.972, 396.652, 254.310, 188.969,  99.371,
     6 1142.785, 1074.586, 735.147, 458.285, 285.026, 207.264, 105.148,
     7 1347.333, 1264.871, 855.442, 522.782, 316.350, 225.537, 110.757,
     8 1562.531, 1464.861, 980.976, 589.175, 347.855, 243.577, 116.156,
     9 1784.982, 1671.409,1109.826, 656.508, 379.152, 261.196, 121.309/
      data pf504/
     1 2011.326, 1881.411,1240.121, 723.879, 409.894, 278.241, 126.192,
     2 2238.393, 2091.940,1370.126, 790.477, 439.785, 294.590, 130.789,
     3 2463.316, 2300.356,1498.291, 855.594, 468.584, 310.148, 135.092,
     4 2683.600, 2504.369,1623.289, 918.642, 496.104, 324.853, 139.098,
     5 2897.165, 2702.067,1744.027, 979.152, 522.209, 338.664, 142.810,
     6 3102.347, 2891.927,1859.649,1036.770, 546.808, 351.564, 146.236,
     7 3297.885, 3072.798,1969.520,1091.248, 569.850, 363.552, 149.386,
     8 3482.889, 3243.870,2073.208,1142.431, 591.321, 374.644, 152.272,
     9 3656.800, 3404.640,2170.460,1190.248, 611.231, 384.865, 154.908/
      data pf505/
     1    7.470,    7.470,   7.470,   7.470,   7.470,   7.470,   7.470,
     2    7.555,    7.555,   7.555,   7.555,   7.555,   7.555,   7.555,
     3    7.644,    7.644,   7.644,   7.644,   7.644,   7.644,   7.644,
     4    7.739,    7.739,   7.739,   7.739,   7.739,   7.739,   7.739,
     5    7.839,    7.839,   7.839,   7.839,   7.839,   7.839,   7.839,
     6    7.946,    7.946,   7.946,   7.946,   7.946,   7.946,   7.946,
     7    8.061,    8.061,   8.061,   8.061,   8.061,   8.061,   8.061,
     8    8.185,    8.185,   8.185,   8.185,   8.185,   8.185,   8.185,
     9    8.319,    8.319,   8.319,   8.319,   8.319,   8.319,   8.319/
      data pf506/
     1    8.464,    8.464,   8.464,   8.464,   8.464,   8.464,   8.464,
     2    8.622,    8.622,   8.622,   8.622,   8.622,   8.622,   8.622,
     3    8.794,    8.794,   8.794,   8.794,   8.794,   8.794,   8.794,
     4    8.981,    8.981,   8.981,   8.981,   8.981,   8.981,   8.981,
     5    9.185,    9.185,   9.185,   9.185,   9.185,   9.185,   9.185,
     6    9.406,    9.406,   9.406,   9.406,   9.406,   9.406,   9.406,
     7    9.647,    9.647,   9.647,   9.647,   9.647,   9.647,   9.647,
     8    9.908,    9.908,   9.908,   9.908,   9.908,   9.908,   9.908,
     9   10.192,   10.192,  10.192,  10.192,  10.192,  10.192,  10.192/
      data pf507/
     1   10.498,   10.498,  10.498,  10.498,  10.498,  10.498,  10.498,
     2   10.828,   10.828,  10.828,  10.828,  10.828,  10.828,  10.828,
     3   11.372,   11.372,  11.372,  11.372,  11.372,  11.372,  11.372,
     4   11.976,   11.976,  11.976,  11.976,  11.976,  11.976,  11.976,
     5   12.645,   12.645,  12.645,  12.645,  12.645,  12.645,  12.645,
     6   13.381,   13.381,  13.381,  13.381,  13.381,  13.381,  13.381,
     7   14.187,   14.187,  14.187,  14.187,  14.187,  14.187,  14.187,
     8   15.067,   15.067,  15.067,  15.067,  15.067,  15.067,  15.067,
     9   16.024,   16.024,  16.024,  16.024,  16.024,  16.024,  16.024/
      data pf508/
     1   17.062,   17.062,  17.062,  17.062,  17.062,  17.062,  17.062,
     2   18.185,   18.185,  18.185,  18.185,  18.185,  18.185,  18.185,
     3   19.400,   19.400,  19.400,  19.400,  19.400,  19.400,  19.400,
     4   21.649,   21.649,  21.649,  21.649,  21.649,  21.648,  21.648,
     5   24.226,   24.226,  24.226,  24.225,  24.225,  24.224,  24.223,
     6   27.219,   27.219,  27.218,  27.218,  27.216,  27.212,  27.204,
     7   30.778,   30.778,  30.776,  30.773,  30.764,  30.749,  30.717,
     8   35.168,   35.165,  35.161,  35.148,  35.111,  35.054,  34.948,
     9   40.857,   40.847,  40.830,  40.785,  40.657,  40.471,  40.161/
      data pf509/
     1   48.664,   48.634,  48.581,  48.442,  48.056,  47.522,  46.719,
     2   59.971,   59.887,  59.740,  59.363,  58.332,  56.961,  55.081,
     3   76.962,   76.753,  76.390,  75.471,  72.997,  69.816,  65.801,
     4  102.850,  102.378, 101.566,  99.531,  94.134,  87.400,  79.499,
     5  141.993,  141.024, 139.359, 135.224, 124.407, 111.266,  96.811,
     6  199.838,  197.994, 194.838, 187.063, 166.959, 143.113, 118.337,
     7  282.625,  279.356, 273.773, 260.122, 225.195, 184.635, 144.568,
     8  396.889,  391.441, 382.159, 359.616, 302.471, 237.352, 175.837,
     9  548.806,  540.218, 525.618, 490.365, 401.744, 302.437, 212.270/
      data pf510/
     1  743.527,  730.643, 708.780, 656.268, 525.235, 380.578, 253.772,
     2  984.581,  966.084, 934.754, 859.851, 674.174, 471.897, 300.026,
     3 1273.473, 1247.945,1204.769,1101.975, 848.652, 575.925, 350.520,
     4 1609.516, 1575.496,1518.034,1381.737,1047.603, 691.641, 404.588,
     5 1989.891, 1945.947,1871.815,1696.554,1268.901, 817.559, 461.452,
     6 2409.916, 2354.713,2261.686,2042.402,1509.546, 951.847, 520.281,
     7 2863.455, 2795.806,2681.915,2414.155,1765.907,1092.459, 580.234,
     8 3343.391, 3262.303,3125.904,2805.977,2033.980,1237.262, 640.502,
     9 3842.120, 3746.821,3586.638,3211.708,2309.640,1384.155, 700.338/
      data pf511/
     1 4351.992, 4241.940,4057.085,3625.212,2588.854,1531.161, 759.085,
     2 4865.683, 4740.569,4530.540,4040.665,2867.860,1676.494, 816.181,
     3   12.790,   12.790,  12.790,  12.790,  12.790,  12.790,  12.790,
     4   12.985,   12.985,  12.985,  12.985,  12.985,  12.985,  12.985,
     5   13.181,   13.181,  13.181,  13.181,  13.181,  13.181,  13.181,
     6   13.378,   13.378,  13.378,  13.378,  13.378,  13.378,  13.378,
     7   13.576,   13.576,  13.576,  13.576,  13.576,  13.576,  13.576,
     8   13.773,   13.773,  13.773,  13.773,  13.773,  13.773,  13.773,
     9   13.971,   13.971,  13.971,  13.971,  13.971,  13.971,  13.971/
      data pf512/
     1   14.169,   14.169,  14.169,  14.169,  14.169,  14.169,  14.169,
     2   14.366,   14.366,  14.366,  14.366,  14.366,  14.366,  14.366,
     3   14.562,   14.562,  14.562,  14.562,  14.562,  14.562,  14.562,
     4   14.758,   14.758,  14.758,  14.758,  14.758,  14.758,  14.758,
     5   14.953,   14.953,  14.953,  14.953,  14.953,  14.953,  14.953,
     6   15.148,   15.148,  15.148,  15.148,  15.148,  15.148,  15.148,
     7   15.342,   15.342,  15.342,  15.342,  15.342,  15.342,  15.342,
     8   15.537,   15.537,  15.537,  15.537,  15.537,  15.537,  15.537,
     9   15.731,   15.731,  15.731,  15.731,  15.731,  15.731,  15.731/
      data pf513/
     1   15.925,   15.925,  15.925,  15.925,  15.925,  15.925,  15.925,
     2   16.120,   16.120,  16.120,  16.120,  16.120,  16.120,  16.120,
     3   16.317,   16.317,  16.317,  16.317,  16.317,  16.317,  16.317,
     4   16.515,   16.515,  16.515,  16.515,  16.515,  16.515,  16.515,
     5   16.818,   16.818,  16.818,  16.818,  16.818,  16.818,  16.818,
     6   17.128,   17.128,  17.128,  17.128,  17.128,  17.128,  17.128,
     7   17.448,   17.448,  17.448,  17.448,  17.448,  17.448,  17.448,
     8   17.780,   17.780,  17.780,  17.780,  17.780,  17.780,  17.780,
     9   18.127,   18.127,  18.127,  18.127,  18.127,  18.127,  18.127/
      data pf514/
     1   18.492,   18.492,  18.492,  18.492,  18.492,  18.492,  18.492,
     2   18.876,   18.876,  18.876,  18.876,  18.876,  18.876,  18.876,
     3   19.282,   19.282,  19.282,  19.282,  19.282,  19.282,  19.282,
     4   19.712,   19.712,  19.712,  19.712,  19.712,  19.712,  19.712,
     5   20.168,   20.168,  20.168,  20.168,  20.168,  20.168,  20.168,
     6   20.994,   20.994,  20.994,  20.994,  20.994,  20.994,  20.994,
     7   21.917,   21.917,  21.917,  21.917,  21.917,  21.917,  21.917,
     8   22.958,   22.958,  22.958,  22.958,  22.958,  22.958,  22.958,
     9   24.155,   24.155,  24.155,  24.155,  24.155,  24.155,  24.155/
      data pf515/
     1   25.564,   25.564,  25.564,  25.564,  25.564,  25.564,  25.564,
     2   27.271,   27.271,  27.271,  27.271,  27.271,  27.271,  27.271,
     3   29.401,   29.401,  29.401,  29.401,  29.401,  29.401,  29.400,
     4   32.137,   32.137,  32.137,  32.137,  32.137,  32.136,  32.133,
     5   35.751,   35.751,  35.751,  35.750,  35.749,  35.742,  35.731,
     6   40.667,   40.664,  40.663,  40.660,  40.655,  40.626,  40.577,
     7   47.580,   47.570,  47.564,  47.554,  47.531,  47.421,  47.245,
     8   57.683,   57.650,  57.627,  57.593,  57.517,  57.153,  56.595,
     9   73.016,   72.914,  72.847,  72.743,  72.517,  71.459,  69.906/
      data pf516/
     1   96.943,   96.667,  96.487,  96.207,  95.611,  92.875,  89.002,
     2  134.675,  134.009, 133.573, 132.901, 131.485, 125.098, 116.359,
     3  193.714,  192.249, 191.293, 189.824, 186.761, 173.166, 155.111,
     4  284.002,  281.046, 279.121, 276.171, 270.082, 243.427, 208.951,
     5  417.663,  412.136, 408.543, 403.055, 391.820, 343.245, 281.879,
     6  608.239,  598.586, 592.320, 582.774, 563.380, 480.452, 377.852,
     7  869.510,  853.643, 843.356, 827.722, 796.172, 662.595, 500.372,
     8 1214.077, 1189.368,1173.366,1149.100,1100.420, 896.125, 652.085,
     9 1651.956, 1615.287,1591.563,1555.656,1484.006,1185.663, 834.473/
      data pf517/
     1 2189.430, 2137.298,2103.600,2052.684,1951.568,1533.453,1047.668,
     2 2828.329, 2756.996,2710.924,2641.417,2503.962,1939.108,1290.422,
     3 3565.824, 3471.490,3410.605,3318.877,3138.160,2399.623,1560.212,
     4 4394.699, 4273.682,4195.626,4078.169,3847.539,2909.659,1853.454,
     5   15.654,   15.654,  15.654,  15.654,  15.654,  15.654,  15.654,
     6   15.938,   15.938,  15.938,  15.938,  15.938,  15.938,  15.938,
     7   16.224,   16.224,  16.224,  16.224,  16.224,  16.224,  16.224,
     8   16.512,   16.512,  16.512,  16.512,  16.512,  16.512,  16.512,
     9   16.801,   16.801,  16.801,  16.801,  16.801,  16.801,  16.801/
      data pf518/
     1   17.091,   17.091,  17.091,  17.091,  17.091,  17.091,  17.091,
     2   17.380,   17.380,  17.380,  17.380,  17.380,  17.380,  17.380,
     3   17.669,   17.669,  17.669,  17.669,  17.669,  17.669,  17.669,
     4   17.957,   17.957,  17.957,  17.957,  17.957,  17.957,  17.957,
     5   18.244,   18.244,  18.244,  18.244,  18.244,  18.244,  18.244,
     6   18.529,   18.529,  18.529,  18.529,  18.529,  18.529,  18.529,
     7   18.813,   18.813,  18.813,  18.813,  18.813,  18.813,  18.813,
     8   19.095,   19.095,  19.095,  19.095,  19.095,  19.095,  19.095,
     9   19.375,   19.375,  19.375,  19.375,  19.375,  19.375,  19.375/
      data pf519/
     1   19.654,   19.654,  19.654,  19.654,  19.654,  19.654,  19.654,
     2   19.932,   19.932,  19.932,  19.932,  19.932,  19.932,  19.932,
     3   20.209,   20.209,  20.209,  20.209,  20.209,  20.209,  20.209,
     4   20.486,   20.486,  20.486,  20.486,  20.486,  20.486,  20.486,
     5   20.763,   20.763,  20.763,  20.763,  20.763,  20.763,  20.763,
     6   21.043,   21.043,  21.043,  21.043,  21.043,  21.043,  21.043,
     7   21.468,   21.468,  21.468,  21.468,  21.468,  21.468,  21.468,
     8   21.905,   21.905,  21.905,  21.905,  21.905,  21.905,  21.905,
     9   22.359,   22.359,  22.359,  22.359,  22.359,  22.359,  22.359/
      data pf520/
     1   22.836,   22.836,  22.836,  22.836,  22.836,  22.836,  22.836,
     2   23.345,   23.345,  23.345,  23.345,  23.345,  23.345,  23.345,
     3   23.892,   23.892,  23.892,  23.892,  23.892,  23.892,  23.892,
     4   24.487,   24.487,  24.487,  24.487,  24.487,  24.487,  24.487,
     5   25.136,   25.136,  25.136,  25.136,  25.136,  25.136,  25.136,
     6   25.849,   25.849,  25.849,  25.849,  25.849,  25.849,  25.849,
     7   26.633,   26.633,  26.633,  26.633,  26.633,  26.633,  26.633,
     8   28.116,   28.116,  28.116,  28.116,  28.116,  28.116,  28.116,
     9   29.843,   29.843,  29.843,  29.843,  29.843,  29.843,  29.843/
      data pf521/
     1   31.831,   31.831,  31.831,  31.831,  31.831,  31.831,  31.831,
     2   34.091,   34.091,  34.091,  34.091,  34.091,  34.091,  34.091,
     3   36.622,   36.622,  36.622,  36.622,  36.622,  36.622,  36.622,
     4   39.418,   39.418,  39.418,  39.418,  39.418,  39.418,  39.418,
     5   42.472,   42.472,  42.472,  42.472,  42.472,  42.472,  42.472,
     6   45.782,   45.782,  45.782,  45.782,  45.782,  45.782,  45.782,
     7   49.368,   49.368,  49.368,  49.368,  49.368,  49.368,  49.368,
     8   53.289,   53.289,  53.289,  53.289,  53.289,  53.289,  53.289,
     9   57.670,   57.670,  57.670,  57.670,  57.670,  57.670,  57.670/
      data pf522/
     1   62.749,   62.749,  62.749,  62.749,  62.748,  62.747,  62.744,
     2   68.942,   68.941,  68.940,  68.940,  68.934,  68.931,  68.915,
     3   76.976,   76.972,  76.970,  76.967,  76.941,  76.926,  76.855,
     4   88.130,   88.117,  88.107,  88.094,  87.995,  87.937,  87.674,
     5  104.659,  104.614, 104.582, 104.536, 104.202, 104.009, 103.164,
     6  130.458,  130.323, 130.227, 130.092, 129.104, 128.543, 126.146,
     7  171.947,  171.589, 171.333, 170.975, 168.381, 166.926, 160.859,
     8  239.038,  238.182, 237.572, 236.720, 230.581, 227.184, 213.297,
     9  345.913,  344.052, 342.726, 340.885, 327.657, 320.423, 291.375/
      data pf523/
     1  511.278,  507.561, 504.915, 501.251, 475.034, 460.842, 404.766,
     2  757.814,  750.928, 746.029, 739.266, 691.028, 665.155, 564.370,
     3 1110.723, 1098.790,1090.308,1078.630, 995.569, 951.382, 781.425,
     4 1595.503, 1576.024,1562.192,1543.187,1408.373,1337.173,1066.397,
     5 2235.316, 2205.172,2183.780,2154.448,1946.862,1837.937,1427.834,
     6 3048.406, 3003.921,2972.371,2929.188,2624.204,2465.094,1871.380,
     7   15.655,   15.655,  15.655,  15.655,  15.655,  15.655,  15.655,
     8   15.911,   15.911,  15.911,  15.911,  15.911,  15.911,  15.911,
     9   16.166,   16.166,  16.166,  16.166,  16.166,  16.166,  16.166/
      data pf524/
     1   16.419,   16.419,  16.419,  16.419,  16.419,  16.419,  16.419,
     2   16.670,   16.670,  16.670,  16.670,  16.670,  16.670,  16.670,
     3   16.918,   16.918,  16.918,  16.918,  16.918,  16.918,  16.918,
     4   17.164,   17.164,  17.164,  17.164,  17.164,  17.164,  17.164,
     5   17.406,   17.406,  17.406,  17.406,  17.406,  17.406,  17.406,
     6   17.645,   17.645,  17.645,  17.645,  17.645,  17.645,  17.645,
     7   17.881,   17.881,  17.881,  17.881,  17.881,  17.881,  17.881,
     8   18.112,   18.112,  18.112,  18.112,  18.112,  18.112,  18.112,
     9   18.340,   18.340,  18.340,  18.340,  18.340,  18.340,  18.340/
      data pf525/
     1   18.563,   18.563,  18.563,  18.563,  18.563,  18.563,  18.563,
     2   18.782,   18.782,  18.782,  18.782,  18.782,  18.782,  18.782,
     3   18.997,   18.997,  18.997,  18.997,  18.997,  18.997,  18.997,
     4   19.207,   19.207,  19.207,  19.207,  19.207,  19.207,  19.207,
     5   19.414,   19.414,  19.414,  19.414,  19.414,  19.414,  19.414,
     6   19.616,   19.616,  19.616,  19.616,  19.616,  19.616,  19.616,
     7   19.816,   19.816,  19.816,  19.816,  19.816,  19.816,  19.816,
     8   20.012,   20.012,  20.012,  20.012,  20.012,  20.012,  20.012,
     9   20.302,   20.302,  20.302,  20.302,  20.302,  20.302,  20.302/
      data pf526/
     1   20.590,   20.590,  20.590,  20.590,  20.590,  20.590,  20.590,
     2   20.881,   20.881,  20.881,  20.881,  20.881,  20.881,  20.881,
     3   21.180,   21.180,  21.180,  21.180,  21.180,  21.180,  21.180,
     4   21.494,   21.494,  21.494,  21.494,  21.494,  21.494,  21.494,
     5   21.832,   21.832,  21.832,  21.832,  21.832,  21.832,  21.832,
     6   22.205,   22.205,  22.205,  22.205,  22.205,  22.205,  22.205,
     7   22.624,   22.624,  22.624,  22.624,  22.624,  22.624,  22.624,
     8   23.103,   23.103,  23.103,  23.103,  23.103,  23.103,  23.103,
     9   23.655,   23.655,  23.655,  23.655,  23.655,  23.655,  23.655/
      data pf527/
     1   24.784,   24.784,  24.784,  24.784,  24.784,  24.784,  24.784,
     2   26.234,   26.234,  26.234,  26.234,  26.234,  26.234,  26.234,
     3   28.080,   28.080,  28.080,  28.080,  28.080,  28.080,  28.080,
     4   30.387,   30.387,  30.387,  30.387,  30.387,  30.387,  30.387,
     5   33.208,   33.208,  33.208,  33.208,  33.208,  33.208,  33.208,
     6   36.583,   36.583,  36.583,  36.583,  36.583,  36.583,  36.583,
     7   40.533,   40.533,  40.533,  40.533,  40.533,  40.533,  40.533,
     8   45.059,   45.059,  45.059,  45.059,  45.059,  45.059,  45.059,
     9   50.147,   50.147,  50.147,  50.147,  50.147,  50.147,  50.147/
      data pf528/
     1   55.766,   55.766,  55.766,  55.766,  55.766,  55.766,  55.766,
     2   61.881,   61.881,  61.881,  61.881,  61.881,  61.881,  61.881,
     3   68.462,   68.462,  68.462,  68.462,  68.462,  68.462,  68.462,
     4   75.511,   75.511,  75.511,  75.511,  75.511,  75.511,  75.511,
     5   83.097,   83.097,  83.097,  83.097,  83.097,  83.097,  83.096,
     6   91.415,   91.414,  91.414,  91.414,  91.413,  91.411,  91.410,
     7  100.880,  100.880, 100.877, 100.876, 100.872, 100.863, 100.855,
     8  112.301,  112.298, 112.288, 112.280, 112.264, 112.222, 112.188,
     9  127.166,  127.156, 127.118, 127.087, 127.025, 126.868, 126.741/
      data pf529/
     1  148.137,  148.106, 147.977, 147.873, 147.668, 147.156, 146.753,
     2  179.776,  179.683, 179.307, 179.003, 178.408, 176.937, 175.804,
     3  229.476,  229.232, 228.252, 227.463, 225.923, 222.157, 219.311,
     4  308.435,  307.861, 305.560, 303.711, 300.117, 291.411, 284.946,
     5  432.375,  431.147, 426.223, 422.272, 414.627, 396.251, 382.820,
     6  621.674,  619.251, 609.551, 601.781, 586.799, 551.041, 525.266,
     7  900.626,  896.191, 878.439, 864.241, 836.949, 772.229, 726.149,
     8 1295.791, 1288.186,1257.766,1233.468,1186.895,1077.070, 999.733,
     9    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000/
      data pf530/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     5    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     8    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     9    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000/
      data pf531/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     3    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     4    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     5    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     6    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     7    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     8    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     9    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000/
      data pf532/
     1    6.000,    6.000,   6.000,   6.000,   6.000,   6.000,   6.000,
     2    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     3    6.001,    6.001,   6.001,   6.001,   6.001,   6.001,   6.001,
     4    6.002,    6.002,   6.002,   6.002,   6.002,   6.002,   6.002,
     5    6.005,    6.005,   6.005,   6.005,   6.005,   6.005,   6.005,
     6    6.009,    6.009,   6.009,   6.009,   6.009,   6.009,   6.009,
     7    6.016,    6.016,   6.016,   6.016,   6.016,   6.016,   6.016,
     8    6.028,    6.028,   6.028,   6.028,   6.028,   6.028,   6.028,
     9    6.048,    6.048,   6.048,   6.048,   6.048,   6.048,   6.048/
      data pf533/
     1    6.078,    6.078,   6.078,   6.078,   6.078,   6.078,   6.078,
     2    6.124,    6.124,   6.124,   6.124,   6.124,   6.124,   6.124,
     3    6.251,    6.251,   6.251,   6.251,   6.251,   6.251,   6.251,
     4    6.478,    6.478,   6.478,   6.478,   6.478,   6.478,   6.478,
     5    6.857,    6.857,   6.857,   6.857,   6.857,   6.857,   6.857,
     6    7.457,    7.457,   7.457,   7.457,   7.457,   7.457,   7.457,
     7    8.362,    8.362,   8.362,   8.362,   8.362,   8.362,   8.362,
     8    9.667,    9.667,   9.667,   9.667,   9.667,   9.667,   9.667,
     9   11.476,   11.476,  11.476,  11.476,  11.476,  11.476,  11.476/
      data pf534/
     1   13.889,   13.889,  13.889,  13.889,  13.889,  13.889,  13.889,
     2   16.998,   16.998,  16.998,  16.998,  16.998,  16.998,  16.998,
     3   20.881,   20.881,  20.881,  20.881,  20.881,  20.881,  20.881,
     4   25.588,   25.588,  25.588,  25.588,  25.588,  25.588,  25.588,
     5   31.142,   31.142,  31.142,  31.142,  31.142,  31.142,  31.142,
     6   37.538,   37.538,  37.538,  37.538,  37.538,  37.538,  37.538,
     7   44.746,   44.746,  44.746,  44.746,  44.746,  44.746,  44.746,
     8   52.723,   52.723,  52.723,  52.723,  52.723,  52.723,  52.723,
     9   61.438,   61.438,  61.438,  61.438,  61.438,  61.438,  61.438/
      data pf535/
     1   70.913,   70.913,  70.913,  70.913,  70.912,  70.912,  70.911,
     2   81.293,   81.293,  81.293,  81.292,  81.292,  81.290,  81.285,
     3   92.989,   92.989,  92.989,  92.985,  92.982,  92.976,  92.949,
     4  106.945,  106.945, 106.945, 106.926, 106.911, 106.884, 106.765,
     5  125.159,  125.159, 125.158, 125.084, 125.027, 124.924, 124.475,
     6  151.611,  151.609, 151.607, 151.357, 151.168, 150.828, 149.363,
     7  193.715,  193.708, 193.704, 192.970, 192.414, 191.429, 187.228,
     8  264.259,  264.239, 264.230, 262.312, 260.864, 258.318, 247.576,
     9  383.518,  383.471, 383.449, 378.937, 375.537, 369.603, 344.801/
      data pf536/
     1  580.962,  580.860, 580.814, 571.141, 563.864, 551.249, 498.964,
     2   10.541,   10.541,  10.541,  10.541,  10.541,  10.541,  10.541,
     3   10.886,   10.886,  10.886,  10.886,  10.886,  10.886,  10.886,
     4   11.234,   11.234,  11.234,  11.234,  11.234,  11.234,  11.234,
     5   11.581,   11.581,  11.581,  11.581,  11.581,  11.581,  11.581,
     6   11.929,   11.929,  11.929,  11.929,  11.929,  11.929,  11.929,
     7   12.276,   12.276,  12.276,  12.276,  12.276,  12.276,  12.276,
     8   12.622,   12.622,  12.622,  12.622,  12.622,  12.622,  12.622,
     9   12.966,   12.966,  12.966,  12.966,  12.966,  12.966,  12.966/
      data pf537/
     1   13.308,   13.308,  13.308,  13.308,  13.308,  13.308,  13.308,
     2   13.647,   13.647,  13.647,  13.647,  13.647,  13.647,  13.647,
     3   13.983,   13.983,  13.983,  13.983,  13.983,  13.983,  13.983,
     4   14.315,   14.315,  14.315,  14.315,  14.315,  14.315,  14.315,
     5   14.643,   14.643,  14.643,  14.643,  14.643,  14.643,  14.643,
     6   14.966,   14.966,  14.966,  14.966,  14.966,  14.966,  14.966,
     7   15.285,   15.285,  15.285,  15.285,  15.285,  15.285,  15.285,
     8   15.598,   15.598,  15.598,  15.598,  15.598,  15.598,  15.598,
     9   15.906,   15.906,  15.906,  15.906,  15.906,  15.906,  15.906/
      data pf538/
     1   16.209,   16.209,  16.209,  16.209,  16.209,  16.209,  16.209,
     2   16.506,   16.506,  16.506,  16.506,  16.506,  16.506,  16.506,
     3   16.798,   16.798,  16.798,  16.798,  16.798,  16.798,  16.798,
     4   17.225,   17.225,  17.225,  17.225,  17.225,  17.225,  17.225,
     5   17.641,   17.641,  17.641,  17.641,  17.641,  17.641,  17.641,
     6   18.048,   18.048,  18.048,  18.048,  18.048,  18.048,  18.048,
     7   18.448,   18.448,  18.448,  18.448,  18.448,  18.448,  18.448,
     8   18.845,   18.845,  18.845,  18.845,  18.845,  18.845,  18.845,
     9   19.244,   19.244,  19.244,  19.244,  19.244,  19.244,  19.244/
      data pf539/
     1   19.653,   19.653,  19.653,  19.653,  19.653,  19.653,  19.653,
     2   20.079,   20.079,  20.079,  20.079,  20.079,  20.079,  20.079,
     3   20.532,   20.532,  20.532,  20.532,  20.532,  20.532,  20.532,
     4   21.026,   21.026,  21.026,  21.026,  21.026,  21.026,  21.026,
     5   21.972,   21.972,  21.972,  21.972,  21.972,  21.972,  21.972,
     6   23.134,   23.134,  23.134,  23.134,  23.134,  23.134,  23.134,
     7   24.587,   24.587,  24.587,  24.587,  24.587,  24.587,  24.587,
     8   26.408,   26.408,  26.408,  26.408,  26.408,  26.408,  26.408,
     9   28.666,   28.666,  28.666,  28.666,  28.666,  28.666,  28.666/
      data pf540/
     1   31.425,   31.425,  31.425,  31.425,  31.425,  31.425,  31.425,
     2   34.728,   34.728,  34.728,  34.728,  34.728,  34.728,  34.728,
     3   38.606,   38.606,  38.606,  38.606,  38.606,  38.606,  38.606,
     4   43.065,   43.065,  43.065,  43.065,  43.065,  43.065,  43.065,
     5   48.094,   48.094,  48.094,  48.094,  48.094,  48.094,  48.094,
     6   53.661,   53.661,  53.661,  53.661,  53.661,  53.661,  53.661,
     7   59.717,   59.717,  59.717,  59.717,  59.717,  59.717,  59.717,
     8   66.197,   66.197,  66.197,  66.197,  66.197,  66.197,  66.197,
     9   73.026,   73.026,  73.026,  73.026,  73.026,  73.026,  73.026/
      data pf541/
     1   80.126,   80.126,  80.126,  80.126,  80.126,  80.126,  80.126,
     2   87.418,   87.418,  87.418,  87.418,  87.418,  87.418,  87.418,
     3   94.843,   94.843,  94.843,  94.843,  94.843,  94.843,  94.843,
     4  102.373,  102.373, 102.373, 102.373, 102.373, 102.373, 102.373,
     5  110.050,  110.050, 110.050, 110.050, 110.050, 110.050, 110.050,
     6  118.043,  118.043, 118.043, 118.043, 118.043, 118.043, 118.040,
     7  126.756,  126.756, 126.756, 126.756, 126.754, 126.753, 126.741,
     8  137.050,  137.050, 137.050, 137.050, 137.039, 137.038, 136.984,
     9  150.652,  150.652, 150.651, 150.650, 150.608, 150.604, 150.407/
      data pf542/
     1  170.855,  170.854, 170.853, 170.850, 170.713, 170.698, 170.072,
     2  203.565,  203.563, 203.561, 203.550, 203.160, 203.117, 201.360,
     3  258.613,  258.607, 258.601, 258.574, 257.579, 257.469, 253.063,
     4    9.538,    9.538,   9.538,   9.538,   9.538,   9.538,   9.538,
     5    9.852,    9.852,   9.852,   9.852,   9.852,   9.852,   9.852,
     6   10.173,   10.173,  10.173,  10.173,  10.173,  10.173,  10.173,
     7   10.502,   10.502,  10.502,  10.502,  10.502,  10.502,  10.502,
     8   10.837,   10.837,  10.837,  10.837,  10.837,  10.837,  10.837,
     9   11.178,   11.178,  11.178,  11.178,  11.178,  11.178,  11.178/
      data pf543/
     1   11.524,   11.524,  11.524,  11.524,  11.524,  11.524,  11.524,
     2   11.875,   11.875,  11.875,  11.875,  11.875,  11.875,  11.875,
     3   12.230,   12.230,  12.230,  12.230,  12.230,  12.230,  12.230,
     4   12.588,   12.588,  12.588,  12.588,  12.588,  12.588,  12.588,
     5   12.948,   12.948,  12.948,  12.948,  12.948,  12.948,  12.948,
     6   13.311,   13.311,  13.311,  13.311,  13.311,  13.311,  13.311,
     7   13.675,   13.675,  13.675,  13.675,  13.675,  13.675,  13.675,
     8   14.039,   14.039,  14.039,  14.039,  14.039,  14.039,  14.039,
     9   14.404,   14.404,  14.404,  14.404,  14.404,  14.404,  14.404/
      data pf544/
     1   14.769,   14.769,  14.769,  14.769,  14.769,  14.769,  14.769,
     2   15.133,   15.133,  15.133,  15.133,  15.133,  15.133,  15.133,
     3   15.496,   15.496,  15.496,  15.496,  15.496,  15.496,  15.496,
     4   15.857,   15.857,  15.857,  15.857,  15.857,  15.857,  15.857,
     5   16.218,   16.218,  16.218,  16.218,  16.218,  16.218,  16.218,
     6   16.755,   16.755,  16.755,  16.755,  16.755,  16.755,  16.755,
     7   17.290,   17.290,  17.290,  17.290,  17.290,  17.290,  17.290,
     8   17.824,   17.824,  17.824,  17.824,  17.824,  17.824,  17.824,
     9   18.360,   18.360,  18.360,  18.360,  18.360,  18.360,  18.360/
      data pf545/
     1   18.899,   18.899,  18.899,  18.899,  18.899,  18.899,  18.899,
     2   19.448,   19.448,  19.448,  19.448,  19.448,  19.448,  19.448,
     3   20.010,   20.010,  20.010,  20.010,  20.010,  20.010,  20.010,
     4   20.592,   20.592,  20.592,  20.592,  20.592,  20.592,  20.592,
     5   21.202,   21.202,  21.202,  21.202,  21.202,  21.202,  21.202,
     6   21.846,   21.846,  21.846,  21.846,  21.846,  21.846,  21.846,
     7   23.018,   23.018,  23.018,  23.018,  23.018,  23.018,  23.018,
     8   24.347,   24.347,  24.347,  24.347,  24.347,  24.347,  24.347,
     9   25.866,   25.866,  25.866,  25.866,  25.866,  25.866,  25.866/
      data pf546/
     1   27.608,   27.608,  27.608,  27.608,  27.608,  27.608,  27.608,
     2   29.596,   29.596,  29.596,  29.596,  29.596,  29.596,  29.596,
     3   31.843,   31.843,  31.843,  31.843,  31.843,  31.843,  31.843,
     4   34.353,   34.353,  34.353,  34.353,  34.353,  34.353,  34.353,
     5   37.120,   37.120,  37.120,  37.120,  37.120,  37.120,  37.120,
     6   40.127,   40.127,  40.127,  40.127,  40.127,  40.127,  40.127,
     7   43.351,   43.351,  43.351,  43.351,  43.351,  43.351,  43.351,
     8   46.760,   46.760,  46.760,  46.760,  46.760,  46.760,  46.760,
     9   50.319,   50.319,  50.319,  50.319,  50.319,  50.319,  50.319/
      data pf547/
     1   53.988,   53.988,  53.988,  53.988,  53.988,  53.988,  53.988,
     2   57.726,   57.726,  57.726,  57.726,  57.726,  57.726,  57.726,
     3   61.494,   61.494,  61.494,  61.494,  61.494,  61.494,  61.494,
     4   65.255,   65.255,  65.255,  65.255,  65.255,  65.255,  65.255,
     5   68.983,   68.983,  68.983,  68.983,  68.983,  68.983,  68.983,
     6   72.666,   72.666,  72.666,  72.666,  72.666,  72.666,  72.666,
     7   76.329,   76.329,  76.329,  76.329,  76.329,  76.329,  76.329,
     8   80.060,   80.060,  80.060,  80.060,  80.060,  80.060,  80.060,
     9   84.053,   84.053,  84.053,  84.053,  84.053,  84.053,  84.053/
      data pf548/
     1   88.676,   88.676,  88.676,  88.676,  88.676,  88.676,  88.676,
     2   94.564,   94.564,  94.564,  94.564,  94.564,  94.564,  94.563,
     3  102.781,  102.781, 102.781, 102.781, 102.781, 102.779, 102.777,
     4  115.070,  115.070, 115.070, 115.069, 115.068, 115.064, 115.057,
     5  134.233,  134.233, 134.233, 134.231, 134.229, 134.214, 134.194,
     6    7.464,    7.464,   7.464,   7.464,   7.464,   7.464,   7.464,
     7    7.652,    7.652,   7.652,   7.652,   7.652,   7.652,   7.652,
     8    7.848,    7.848,   7.848,   7.848,   7.848,   7.848,   7.848,
     9    8.050,    8.050,   8.050,   8.050,   8.050,   8.050,   8.050/
      data pf549/
     1    8.259,    8.259,   8.259,   8.259,   8.259,   8.259,   8.259,
     2    8.474,    8.474,   8.474,   8.474,   8.474,   8.474,   8.474,
     3    8.695,    8.695,   8.695,   8.695,   8.695,   8.695,   8.695,
     4    8.922,    8.922,   8.922,   8.922,   8.922,   8.922,   8.922,
     5    9.153,    9.153,   9.153,   9.153,   9.153,   9.153,   9.153,
     6    9.389,    9.389,   9.389,   9.389,   9.389,   9.389,   9.389,
     7    9.629,    9.629,   9.629,   9.629,   9.629,   9.629,   9.629,
     8    9.872,    9.872,   9.872,   9.872,   9.872,   9.872,   9.872,
     9   10.119,   10.119,  10.119,  10.119,  10.119,  10.119,  10.119/
      data pf550/
     1   10.368,   10.368,  10.368,  10.368,  10.368,  10.368,  10.368,
     2   10.620,   10.620,  10.620,  10.620,  10.620,  10.620,  10.620,
     3   10.873,   10.873,  10.873,  10.873,  10.873,  10.873,  10.873,
     4   11.128,   11.128,  11.128,  11.128,  11.128,  11.128,  11.128,
     5   11.384,   11.384,  11.384,  11.384,  11.384,  11.384,  11.384,
     6   11.641,   11.641,  11.641,  11.641,  11.641,  11.641,  11.641,
     7   11.899,   11.899,  11.899,  11.899,  11.899,  11.899,  11.899,
     8   12.286,   12.286,  12.286,  12.286,  12.286,  12.286,  12.286,
     9   12.675,   12.675,  12.675,  12.675,  12.675,  12.675,  12.675/
      data pf551/
     1   13.065,   13.065,  13.065,  13.065,  13.065,  13.065,  13.065,
     2   13.456,   13.456,  13.456,  13.456,  13.456,  13.456,  13.456,
     3   13.850,   13.850,  13.850,  13.850,  13.850,  13.850,  13.850,
     4   14.247,   14.247,  14.247,  14.247,  14.247,  14.247,  14.247,
     5   14.650,   14.650,  14.650,  14.650,  14.650,  14.650,  14.650,
     6   15.059,   15.059,  15.059,  15.059,  15.059,  15.059,  15.059,
     7   15.477,   15.477,  15.477,  15.477,  15.477,  15.477,  15.477,
     8   15.905,   15.905,  15.905,  15.905,  15.905,  15.905,  15.905,
     9   16.648,   16.648,  16.648,  16.648,  16.648,  16.648,  16.648/
      data pf552/
     1   17.433,   17.433,  17.433,  17.433,  17.433,  17.433,  17.433,
     2   18.269,   18.269,  18.269,  18.269,  18.269,  18.269,  18.269,
     3   19.161,   19.161,  19.161,  19.161,  19.161,  19.161,  19.161,
     4   20.108,   20.108,  20.108,  20.108,  20.108,  20.108,  20.108,
     5   21.111,   21.111,  21.111,  21.111,  21.111,  21.111,  21.111,
     6   22.165,   22.165,  22.165,  22.165,  22.165,  22.165,  22.165,
     7   23.262,   23.262,  23.262,  23.262,  23.262,  23.262,  23.262,
     8   24.394,   24.394,  24.394,  24.394,  24.394,  24.394,  24.394,
     9   25.548,   25.548,  25.548,  25.548,  25.548,  25.548,  25.548/
      data pf553/
     1   26.715,   26.715,  26.715,  26.715,  26.715,  26.715,  26.715,
     2   27.880,   27.880,  27.880,  27.880,  27.880,  27.880,  27.880,
     3   29.034,   29.034,  29.034,  29.034,  29.034,  29.034,  29.034,
     4   30.166,   30.166,  30.166,  30.166,  30.166,  30.166,  30.166,
     5   31.267,   31.267,  31.267,  31.267,  31.267,  31.267,  31.267,
     6   32.332,   32.332,  32.332,  32.332,  32.332,  32.332,  32.332,
     7   33.358,   33.358,  33.358,  33.358,  33.358,  33.358,  33.358,
     8   34.354,   34.354,  34.354,  34.354,  34.354,  34.354,  34.354,
     9   35.348,   35.348,  35.348,  35.348,  35.348,  35.348,  35.348/
      data pf554/
     1   36.396,   36.396,  36.396,  36.396,  36.396,  36.396,  36.396,
     2   37.608,   37.608,  37.608,  37.608,  37.608,  37.608,  37.608,
     3   39.157,   39.157,  39.157,  39.157,  39.157,  39.157,  39.157,
     4   41.311,   41.311,  41.311,  41.311,  41.311,  41.311,  41.311,
     5   44.442,   44.442,  44.442,  44.442,  44.442,  44.442,  44.442,
     6   49.065,   49.065,  49.065,  49.065,  49.065,  49.065,  49.065,
     7   55.870,   55.870,  55.870,  55.870,  55.870,  55.870,  55.870,
     8    4.672,    4.672,   4.672,   4.672,   4.672,   4.672,   4.672,
     9    4.742,    4.742,   4.742,   4.742,   4.742,   4.742,   4.742/
      data pf555/
     1    4.815,    4.815,   4.815,   4.815,   4.815,   4.815,   4.815,
     2    4.892,    4.892,   4.892,   4.892,   4.892,   4.892,   4.892,
     3    4.972,    4.972,   4.972,   4.972,   4.972,   4.972,   4.972,
     4    5.055,    5.055,   5.055,   5.055,   5.055,   5.055,   5.055,
     5    5.141,    5.141,   5.141,   5.141,   5.141,   5.141,   5.141,
     6    5.229,    5.229,   5.229,   5.229,   5.229,   5.229,   5.229,
     7    5.320,    5.320,   5.320,   5.320,   5.320,   5.320,   5.320,
     8    5.413,    5.413,   5.413,   5.413,   5.413,   5.413,   5.413,
     9    5.508,    5.508,   5.508,   5.508,   5.508,   5.508,   5.508/
      data pf556/
     1    5.605,    5.605,   5.605,   5.605,   5.605,   5.605,   5.605,
     2    5.703,    5.703,   5.703,   5.703,   5.703,   5.703,   5.703,
     3    5.802,    5.802,   5.802,   5.802,   5.802,   5.802,   5.802,
     4    5.903,    5.903,   5.903,   5.903,   5.903,   5.903,   5.903,
     5    6.004,    6.004,   6.004,   6.004,   6.004,   6.004,   6.004,
     6    6.105,    6.105,   6.105,   6.105,   6.105,   6.105,   6.105,
     7    6.207,    6.207,   6.207,   6.207,   6.207,   6.207,   6.207,
     8    6.308,    6.308,   6.308,   6.308,   6.308,   6.308,   6.308,
     9    6.410,    6.410,   6.410,   6.410,   6.410,   6.410,   6.410/
      data pf557/
     1    6.561,    6.561,   6.561,   6.561,   6.561,   6.561,   6.561,
     2    6.711,    6.711,   6.711,   6.711,   6.711,   6.711,   6.711,
     3    6.858,    6.858,   6.858,   6.858,   6.858,   6.858,   6.858,
     4    7.003,    7.003,   7.003,   7.003,   7.003,   7.003,   7.003,
     5    7.145,    7.145,   7.145,   7.145,   7.145,   7.145,   7.145,
     6    7.284,    7.284,   7.284,   7.284,   7.284,   7.284,   7.284,
     7    7.419,    7.419,   7.419,   7.419,   7.419,   7.419,   7.419,
     8    7.549,    7.549,   7.549,   7.549,   7.549,   7.549,   7.549,
     9    7.676,    7.676,   7.676,   7.676,   7.676,   7.676,   7.676/
      data pf558/
     1    7.798,    7.798,   7.798,   7.798,   7.798,   7.798,   7.798,
     2    7.992,    7.992,   7.992,   7.992,   7.992,   7.992,   7.992,
     3    8.173,    8.173,   8.173,   8.173,   8.173,   8.173,   8.173,
     4    8.341,    8.341,   8.341,   8.341,   8.341,   8.341,   8.341,
     5    8.496,    8.496,   8.496,   8.496,   8.496,   8.496,   8.496,
     6    8.640,    8.640,   8.640,   8.640,   8.640,   8.640,   8.640,
     7    8.771,    8.771,   8.771,   8.771,   8.771,   8.771,   8.771,
     8    8.892,    8.892,   8.892,   8.892,   8.892,   8.892,   8.892,
     9    9.001,    9.001,   9.001,   9.001,   9.001,   9.001,   9.001/
      data pf559/
     1    9.101,    9.101,   9.101,   9.101,   9.101,   9.101,   9.101,
     2    9.192,    9.192,   9.192,   9.192,   9.192,   9.192,   9.192,
     3    9.275,    9.275,   9.275,   9.275,   9.275,   9.275,   9.275,
     4    9.349,    9.349,   9.349,   9.349,   9.349,   9.349,   9.349,
     5    9.416,    9.416,   9.416,   9.416,   9.416,   9.416,   9.416,
     6    9.477,    9.477,   9.477,   9.477,   9.477,   9.477,   9.477,
     7    9.532,    9.532,   9.532,   9.532,   9.532,   9.532,   9.532,
     8    9.583,    9.583,   9.583,   9.583,   9.583,   9.583,   9.583,
     9    9.633,    9.633,   9.633,   9.633,   9.633,   9.633,   9.633/
      data pf560/
     1    9.689,    9.689,   9.689,   9.689,   9.689,   9.689,   9.689,
     2    9.766,    9.766,   9.766,   9.766,   9.766,   9.766,   9.766,
     3    9.891,    9.891,   9.891,   9.891,   9.891,   9.891,   9.891,
     4   10.111,   10.111,  10.111,  10.111,  10.111,  10.111,  10.111,
     5   10.499,   10.499,  10.499,  10.499,  10.499,  10.499,  10.499,
     6   11.155,   11.155,  11.155,  11.155,  11.155,  11.155,  11.155,
     7   12.209,   12.209,  12.209,  12.209,  12.209,  12.209,  12.209,
     8   13.817,   13.817,  13.817,  13.817,  13.817,  13.817,  13.817,
     9   16.163,   16.163,  16.163,  16.163,  16.163,  16.163,  16.163/
      data potlo/500.,1000.,2000.,4000.,8000.,16000.,32000./
      data potlolog/2.69897,3.,3.30103,3.60206,3.90309,4.20412,4.50515/
c     if(pftab(1,1,1,1).eq.0.)then
c     open(unit=4,file='ku2a:[kurucz.synthe]pfiron.dat',
c    1status='old',shared,readonly)
c     read(4,1)pftab
c   1 format(26x,7f10.3)
c     endif
c123456789012345678901234567890
c 20.09        56   199526.     4.211     4.211     4.211     4.211     4.211     4.211     4.211
      tlog=tlog8
      potlow=potlow8

      if(tlog.gt.4.) then
       it=(tlog-4.0)/0.05d0 +31.0d0 
       it=min(it,56.d0)
       f=(tlog-(it-31)*.05-4.)/.05
      else if(tlog.lt.3.7) then
       it=(tlog-3.32)/.02+2.
       it=max(it,2.0d0)
       f=(tlog-(it-2)*.02-3.32)/.02
      else
       it=(tlog-3.7)/.03+21.
       f=(tlog-(it-21)*.03-3.7)/.03
      end if

  

      low=1
     
      if(potlow.lt.potlo(low)) then 

       pf=f*pftab(low,it,ion,nelem-19)+
     1   (1.-f)*pftab(low,it-1,ion,nelem-19)
      
      else 

 
        do  low=2,7
          if((potlow.lt.potlo(low)) .or. (low .eq.  7)) then 

           p=(log10(potlow)-potlolog(low-1))/.30103
           pf=p*(f*pftab(low,it,ion,nelem-19)+
     1       (1.-f)*pftab(low,it-1,ion,nelem-19))+
     2       (1.-p)*(f*pftab(low-1,it,ion,nelem-19)+
     3       (1.-f)*pftab(low-1,it-1,ion,nelem-19))
          end if  
         
      end do 

      end if

#endif
      end 

*
