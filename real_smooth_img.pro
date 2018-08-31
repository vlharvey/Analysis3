function real_smooth_img,data,radius
  s=size(data,/dim)
  t=size(data,/type)
  big_data=make_array(s+radius*2,type=t,value=!values.f_nan)
  big_data[radius:radius+s[0]-1,radius:radius+s[1]-1]=data
  big_data=smooth(big_data,radius,/nan)
  return,big_data[radius:radius+s[0]-1,radius:radius+s[1]-1]
end
