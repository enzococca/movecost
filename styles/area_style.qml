<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.42" styleCategories="AllStyleCategories" labelsEnabled="1">
  <renderer-v2 type="singleSymbol" symbollevels="0" enableorderby="0" forceraster="0">
    <symbols>
      <symbol type="fill" name="0" alpha="0.5" clip_to_extent="1" force_rhr="0">
        <layer locked="0" enabled="1" pass="0" class="SimpleFill">
          <Option type="Map">
            <Option type="QString" name="color" value="166,206,227,180"/>
            <Option type="QString" name="style" value="solid"/>
            <Option type="QString" name="outline_color" value="31,120,180,255"/>
            <Option type="QString" name="outline_style" value="solid"/>
            <Option type="QString" name="outline_width" value="0.6"/>
            <Option type="QString" name="outline_width_unit" value="MM"/>
          </Option>
        </layer>
      </symbol>
    </symbols>
  </renderer-v2>
  <labeling type="simple">
    <settings calloutType="simple">
      <text-style fontSize="9" fontFamily="Sans Serif" fontWeight="75" textColor="31,120,180,255" fieldName="if(&quot;area_km2&quot; IS NOT NULL, round(&quot;area_km2&quot;, 2) || ' km²', if(&quot;time_converted&quot; IS NOT NULL, &quot;time_converted&quot;, if(&quot;cost&quot; IS NOT NULL, round(cost, 2) || ' h', '')))">
        <text-buffer bufferSize="1" bufferColor="255,255,255,200" bufferDraw="1"/>
      </text-style>
      <placement placement="0"/>
    </settings>
  </labeling>
  <blendMode>0</blendMode>
</qgis>
