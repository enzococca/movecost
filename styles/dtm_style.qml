<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.42" styleCategories="AllStyleCategories" hasScaleBasedVisibilityFlag="0" minScale="1e+08" maxScale="0">
  <pipe>
    <provider>
      <resampling enabled="false" maxOversampling="2" zoomedOutResamplingMethod="nearestNeighbour" zoomedInResamplingMethod="nearestNeighbour"/>
    </provider>
    <rasterrenderer type="singlebandpseudocolor" band="1" opacity="1" classificationMin="0" classificationMax="3000" alphaBand="-1" nodataColor="">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>MinMax</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader colorRampType="INTERPOLATED" classificationMode="1" minimumValue="0" maximumValue="3000" clip="0" labelPrecision="0">
          <colorramp name="[source]" type="gradient">
            <Option type="Map">
              <Option name="color1" type="QString" value="0,97,0,255"/>
              <Option name="color2" type="QString" value="255,255,255,255"/>
              <Option name="discrete" type="QString" value="0"/>
              <Option name="rampType" type="QString" value="gradient"/>
              <Option name="stops" type="QString" value="0.0333333;56,168,0,255:0.0666667;121,201,0,255:0.1;194,230,0,255:0.133333;255,255,0,255:0.2;255,235,176,255:0.3;255,195,128,255:0.4;228,156,93,255:0.5;191,129,45,255:0.6;140,115,100,255:0.7;161,145,141,255:0.8;183,183,183,255:0.9;220,220,220,255"/>
            </Option>
          </colorramp>
          <item alpha="255" label="0" color="#006100" value="0"/>
          <item alpha="255" label="100" color="#38a800" value="100"/>
          <item alpha="255" label="200" color="#79c900" value="200"/>
          <item alpha="255" label="300" color="#c2e600" value="300"/>
          <item alpha="255" label="400" color="#ffff00" value="400"/>
          <item alpha="255" label="600" color="#ffebb0" value="600"/>
          <item alpha="255" label="900" color="#ffc380" value="900"/>
          <item alpha="255" label="1200" color="#e49c5d" value="1200"/>
          <item alpha="255" label="1500" color="#bf812d" value="1500"/>
          <item alpha="255" label="1800" color="#8c7364" value="1800"/>
          <item alpha="255" label="2100" color="#a1918d" value="2100"/>
          <item alpha="255" label="2400" color="#b7b7b7" value="2400"/>
          <item alpha="255" label="2700" color="#dcdcdc" value="2700"/>
          <item alpha="255" label="3000" color="#ffffff" value="3000"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>
    <huesaturation colorizeGreen="128" colorizeRed="255" grayscaleMode="0" saturation="0" colorizeBlue="128" colorizeStrength="100" invertColors="0" colorizeOn="0"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
