## dataStitcher
Stitch images and data from coupled SEM/XEDS


This script stitches SEM images and corresponding XEDS data into raster bricks in R. The script requires xy coordinates for the bottom left corner of the SEM images. One way to obtain these coordinates is by using the photomerge script in Photoshop. 

Here is an example rock surface SEM image, purple box outlines a high resolution transect region:

![Image of rock overview](https://raw.githubusercontent.com/CaitlinCasar/dataStitcher/master/rock_overview.jpg)

The transect is comprised of seven SEM images at 3000X magnification. This is an example of one of the seven images:


![3000X SEM image](https://raw.githubusercontent.com/CaitlinCasar/dataStitcher/master/mag_image.jpg)

and its position in the transect:

![Image of transect](https://raw.githubusercontent.com/CaitlinCasar/dataStitcher/master/transect.jpg)

The script merges the images and XEDS TIFF files into panoramic images:

![Panoramic images](https://raw.githubusercontent.com/CaitlinCasar/dataStitcher/master/elements.jpg)

The final output is a raster brick of the XEDS data and an optional PDF of the superimposed SEM and XEDS panoramic data.
