<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.42" styleCategories="AllStyleCategories" labelsEnabled="1">
  <renderer-v2 type="singleSymbol" symbollevels="0" enableorderby="0" forceraster="0">
    <symbols>
      <symbol type="line" name="0" alpha="1" clip_to_extent="1" force_rhr="0">
        <layer locked="0" enabled="1" pass="0" class="SimpleLine">
          <Option type="Map">
            <Option type="QString" name="line_color" value="34,139,34,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="0.8"/>
            <Option type="QString" name="line_width_unit" value="MM"/>
            <Option type="QString" name="capstyle" value="round"/>
            <Option type="QString" name="joinstyle" value="round"/>
          </Option>
        </layer>
      </symbol>
    </symbols>
  </renderer-v2>
  <labeling type="simple">
    <settings calloutType="simple">
      <text-style fontSize="9" fontFamily="Sans Serif" fontItalic="0" fontWeight="75" textColor="34,139,34,255" fieldName="concat(round(level, 1), ' h')">
        <text-buffer bufferSize="0.8" bufferColor="255,255,255,220" bufferDraw="1"/>
      </text-style>
      <placement placement="3" repeatDistance="100" repeatDistanceUnits="MM" overrunDistance="0"/>
    </settings>
  </labeling>
  <blendMode>0</blendMode>
</qgis>
