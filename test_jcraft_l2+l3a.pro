;
; From: James Craft
; Sent: Friday, January 20, 2017 11:49 AM
;
testDir = '/atmos/harvey/CIPS_data/Datfiles/'
l3Filename = testDir + 'Level_3a_Daisies/cips_sci_3a_2016-357_v05.10_r01.nc'
l2catfile_gz = testDir + 'Level_2/cips_sci_2_orbit_52822_2016-357_v05.10_r01_cat.nc.gz'
l2cldfile_gz = testDir + 'Level_2/cips_sci_2_orbit_52822_2016-357_v05.10_r01_cld.nc.gz'

; read the data
restore, 'read_cips_file_idl84.sav'
l3data=read_cips_file(l3filename)
l2catdata = read_cips_file(l2catfile_gz)
l2clddata = read_cips_file(l2cldfile_gz)

;calculate the bounding box
bbox=[0, 0, 1699, 1699]
km_per_pixel = 7.5D
l3_center_lon = -90.0 ; north
l3_center_lon = 90.0 ; south
bbox_lat_lon, bbox,lat=bboxLat, lon=bboxLon, kmperpix=km_per_pixel, /no_co, center_lon=l3_center_lon, /south

;plot it
map = MAP('Orthographic', CENTER_LATITUDE = -90, CENTER_LONGITUDE = 0, FILL_COLOR = "white")
m1 = MAPCONTINENTS()
zed = plot(bboxLon, bboxLat, linestyle=6, symbol='period', sym_color='green', sym_transparency=97, /overplot)

; pull out the L3 lat/lon/albedo
l3lat=l3data.latitude
l3lon=l3data.longitude
l3alb=l3data.albedo
l3good=where(finite(l3alb) eq 1,ngood)
;extract "good" lat/lon
l3lat_good = l3lat(l3good)
l3lon_good = l3lon(l3good)

;extract "good" bbox lat/lon for comparison
myLat_good = bboxLat(l3good)
myLon_good = bboxLon(l3good)

;draw L3 data
zed = plot(l3lon_good, l3lat_good, linestyle=6, symbol='period', sym_color='red', sym_transparency=97, /overplot)

;work on the L2 data
szagood=where(finite(l2catdata.ZENITH_ANGLE_RAY_PEAK) eq 1)
l2sza=l2catdata.ZENITH_ANGLE_RAY_PEAK[szagood]
l2lat=l2catdata.LATITUDE[szagood]
l2lon=l2catdata.LONGITUDE[szagood]

;convert raw lat to "regular" lat
;l2latasc = where(l2lat lt -90, nl2latasc)
l2latasc = where(abs(l2lat) gt 90.,complement=dsc)
l2lat(l2latasc) = -180.0 - l2lat(l2latasc)

l2good = where(finite(l2clddata.CLOUD_PRESENCE_MAP) eq 1, ncldgood)
l2lat_good = l2lat(l2good)
l2lon_good = l2lon(l2good)

;draw the L2 data
zed = plot(l2lon_good, l2lat_good, linestyle=6, symbol='period', sym_color='blue', sym_transparency=98, /overplot)

end
