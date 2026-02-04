<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.42" styleCategories="AllStyleCategories" hasScaleBasedVisibilityFlag="0" minScale="1e+08" maxScale="0">
  <pipe>
    <provider>
      <resampling enabled="false" maxOversampling="2" zoomedOutResamplingMethod="nearestNeighbour" zoomedInResamplingMethod="nearestNeighbour"/>
    </provider>
    <rasterrenderer type="singlebandpseudocolor" band="1" opacity="0.8" classificationMin="0" classificationMax="100" alphaBand="-1" nodataColor="">
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
        <colorrampshader colorRampType="INTERPOLATED" classificationMode="2" minimumValue="0" maximumValue="100" clip="0" labelPrecision="2">
          <colorramp name="[source]" type="gradient">
            <Option type="Map">
              <Option name="color1" type="QString" value="26,150,65,255"/>
              <Option name="color2" type="QString" value="215,25,28,255"/>
              <Option name="discrete" type="QString" value="0"/>
              <Option name="rampType" type="QString" value="gradient"/>
              <Option name="stops" type="QString" value="0.25;166,217,106,255:0.5;255,255,192,255:0.75;253,174,97,255"/>
            </Option>
          </colorramp>
          <item alpha="255" label="Low cost" color="#1a9641" value="0"/>
          <item alpha="255" label="" color="#a6d96a" value="25"/>
          <item alpha="255" label="Medium" color="#ffffc0" value="50"/>
          <item alpha="255" label="" color="#fdae61" value="75"/>
          <item alpha="255" label="High cost" color="#d7191c" value="100"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>
    <huesaturation colorizeGreen="128" colorizeRed="255" grayscaleMode="0" saturation="0" colorizeBlue="128" colorizeStrength="100" invertColors="0" colorizeOn="0"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
