nframe=0
nskip=0
ifile=''
ofile=''
dir=' '
read,'input file name ',ifile
read,'output file prefix ',ofile
read,'input number of frames ',nframe
read,'input number of skips ',nskip
OPENR, 1,ifile
loadct,39

for n=0,nskip-1 do begin
    H = BYTARR(800, 800)
    READU, 1, H
endfor

for n=0,nframe-1 do begin
    H = BYTARR(800, 800)
    READU, 1, H
    if n le 9 then filename=ofile+strmid(string(n),7,1)+'.gif'
    if n ge 10 and n le 99 then filename=ofile+strmid(string(n),6,2)+'.gif'
    if n ge 100 and n le 999 then filename=ofile+strmid(string(n),5,3)+'.gif'
    write_gif,dir+filename,h
endfor

end


