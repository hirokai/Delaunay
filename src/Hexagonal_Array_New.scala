import ij.measure.{Measurements,ResultsTable}
import ij.plugin._
import ij.plugin.filter.{Analyzer,ParticleAnalyzer}
import ij.plugin.frame.PlugInFrame
import ij._
import ij.gui._
import ij.process._
import java.awt._
import java.awt.geom.{GeneralPath,Point2D}
import java.awt.image._
import java.awt.event._
import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.math._

object Hexagonal_Array_Analysis{
  var instance: Frame = _
}

class Hexagonal_Array_Analysis(t:String) extends PlugInFrame(t){
	private val serialVersionUID = 1L
	
	val MAX_MIN_PARTICLESIZE = 100
	val MAX_CROP = 1000

	def setSbValIfChanged(sb: Scrollbar, i: Int): Unit = {
		if(sb.getValue != i)
		  sb.setValue(i)
	}
	def setTfValIfChanged(tf: TextField, i: Int): Unit = {
		if(tf.getText != i.toString)
		  tf.setText(i.toString)
	}	
	def setTfValIfChanged(tf: TextField, d: Double): Unit = {
	  val E = 0.001
		if(tf.getText.toDouble - d > E)
		  tf.setText(d.toString)	  
	}
	//Parameters for thresholding
	var p_maxThreshold = 255
	def maxThreshold_=(v:Int):Unit ={
	  if(v>=minThreshold && v<=255) p_maxThreshold = v
	  else p_maxThreshold = maxThreshold
	  setTfValIfChanged(tfMaxTh,maxThreshold)
	  setSbValIfChanged(sbMaxTh,maxThreshold)
	  updateThreshold
	}
	def maxThreshold: Int = p_maxThreshold
	var p_minThreshold = 100
	def minThreshold_=(v:Int):Unit ={
	  if(v>=0 && v<=maxThreshold) p_minThreshold = v
	  else p_minThreshold = minThreshold
	  setTfValIfChanged(tfMinTh,v)
	  setSbValIfChanged(sbMinTh,v)
	  updateThreshold
	}
	def minThreshold: Int = p_minThreshold
	var tfMaxTh, tfMinTh: TextField = _
	var tfActual,tfPixels: TextField = _
	var chPreset: Choice = _

	//Parameters for particle detection
	val opt = ParticleAnalyzer.SHOW_MASKS | ParticleAnalyzer.IN_SITU_SHOW;
	val meas = Measurements.AREA | Measurements.CENTER_OF_MASS;
	var particleAnalyzer: MyParticleAnalyzer =_
	var p_minParticle = 5
	def minParticle_=(v:Int):Unit = {
	  if(v>=0 && v<=MAX_MIN_PARTICLESIZE) p_minParticle = v
	  else minParticle = p_minParticle
	  setTfValIfChanged(tfMinParticle,v)
	  setSbValIfChanged(sbMinParticle,v)
	}
	def minParticle: Int = p_minParticle
	var tfMinParticle: TextField = _
	var maxParticle = 10000
	var roi: Rectangle = null
	
	//Parameters for pair detection
	var p_crop = 200: Int
	def crop_=(v:Int):Unit ={
	  if(v>=0 && v<=MAX_CROP) p_crop = v
	  else crop = crop
	  setTfValIfChanged(tfCrop,v)
	  setSbValIfChanged(sbCrop,v)
	  updateCrop
	}
	def crop: Int = p_crop
	var tfCrop: TextField = _
	var sbCrop: Scrollbar = _
	
	var p_actualLength: Double = 500;
	def actualLength_=(v: Double): Unit = {
	  if(v>0) p_actualLength = v
	  else actualLength = actualLength
	  chPreset.select(inPreset(actualLength,numPixelsToThat));
	  updateCrop
	}
	def actualLength: Double =  p_actualLength

	var p_numPixelsToThat: Double = 227;
	def numPixelsToThat_=(v:Double):Unit = {
	  if(v>0) p_numPixelsToThat = v
	  else numPixelsToThat = numPixelsToThat
	  chPreset.select(inPreset(actualLength,numPixelsToThat));
	  updateCrop
	}
	def numPixelsToThat = p_numPixelsToThat

	def factor: Double = actualLength/numPixelsToThat

	var result: SpacingResult = _

	var particles:Array[Point2] = _
	var calculator: SpacingCalculator = _
	var original: ImagePlus = null;
	var original_p: ImageProcessor = _
	var output: ImagePlus = _
	var output_p: ImageProcessor = _
	
	var newTask = true;
	var windowActive = false
	
	var sbMinParticle: Scrollbar = _
	var sbMaxTh: Scrollbar = _
	var sbMinTh: Scrollbar = _
	var taOutput: TextArea = _

	var outputImgWidth = 0;
	var outputImgHeight = 0;

	def this() = {
		this("Hexagonal Array Analysis")
		if (Hexagonal_Array_Analysis.instance!=null) {
			Hexagonal_Array_Analysis.instance.toFront;
		}else{
			Hexagonal_Array_Analysis.instance = this;
			addKeyListener(IJ.getInstance);
		}
	}
	override def run(arg: String): Unit = {
		original = WindowManager.getCurrentImage();
		original_p = original.getProcessor();
		roi = original_p.getRoi();
		prepareGUI
		GUI.center(this);
		setVisible(true);
		windowActive = true
		newTask = true;
		updateThreshold
		updateCrop
	}
	
	def setParametersByMagnification(mag: String): Unit = {
		val (a:Int,b:Int,c:Int) = if(mag == "20k")	(1000,227,400)
		else if(mag =="40k") (500,227,200)
		else if(mag =="80k") (250,227,100)
		else if(mag =="200k") (100,227,40)
		actualLength = a;
		numPixelsToThat = b;
		crop = c
		updateCrop
	}

	def prepareGUI: Unit = {
		val layout = new GridBagLayout
		setLayout(layout)
		val gbc = new GridBagConstraints
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;	gbc.weighty = 1;

		var label = new Label("Max threshold");
		gbc.gridx = 0;	gbc.gridy = 0;	gbc.gridheight = 1;	gbc.gridwidth = 2;
		add(label,gbc);
		
		val sb_max = 255;
		val sb_width = 26;
		sbMaxTh = new Scrollbar(Scrollbar.HORIZONTAL, maxThreshold, sb_width, 0, sb_max+sb_width);
		sbMaxTh.addAdjustmentListener(new AdjustmentListener(){
			def adjustmentValueChanged(arg0: AdjustmentEvent) = {maxThreshold = arg0.getValue}
		});
		gbc.gridx = 2;	gbc.gridy = 0;	gbc.gridwidth = 2;
		add(sbMaxTh,gbc);
		
		tfMaxTh = new TextField(String.valueOf(maxThreshold));
		tfMaxTh.addFocusListener(new FocusListener() {
			override def focusLost(e:FocusEvent): Unit = {
				try{maxThreshold = tfMaxTh.getText.toInt}
				catch{case ex: NumberFormatException => maxThreshold = maxThreshold}
			}
			def focusGained(e:FocusEvent): Unit = {}
		});
		gbc.gridx = 4;	gbc.gridy = 0;	gbc.gridwidth = 1;
		add(tfMaxTh,gbc);

		label = new Label("Min threshold");
		gbc.gridx = 0;	gbc.gridy = 1;	gbc.gridwidth = 2;
		add(label,gbc);
		
		sbMinTh = new Scrollbar(Scrollbar.HORIZONTAL, minThreshold, sb_width, 0, sb_max+sb_width);
		sbMinTh.addAdjustmentListener(new AdjustmentListener(){
			def adjustmentValueChanged(arg0: AdjustmentEvent): Unit = {minThreshold = arg0.getValue}
		});
		gbc.gridx = 2;	gbc.gridy = 1;	gbc.gridwidth = 2;
		add(sbMinTh,gbc);

		tfMinTh = new TextField(String.valueOf(minThreshold));
		tfMinTh.addFocusListener(new FocusListener() {
			def focusLost(e:FocusEvent): Unit = {
				try{minThreshold = tfMinTh.getText.toInt}
				catch{case ex: NumberFormatException => minThreshold = minThreshold}
			}
			def focusGained(e:FocusEvent): Unit = {}
		});
		gbc.gridx = 4;	gbc.gridy = 1;	gbc.gridwidth = 1;
		add(tfMinTh,gbc);

		label = new Label("Min particle pixels");
		label.setAlignment(Label.LEFT);
		gbc.gridx = 0;	gbc.gridy = 2;	gbc.gridwidth = 2;
		add(label,gbc);
		
		sbMinParticle = new Scrollbar(Scrollbar.HORIZONTAL, minParticle, 10, 0, MAX_MIN_PARTICLESIZE+10);
		sbMinParticle.addAdjustmentListener(new AdjustmentListener(){
			def adjustmentValueChanged(arg0: AdjustmentEvent) = {minParticle = arg0.getValue}
		});
		gbc.gridx = 2;	gbc.gridy = 2;	gbc.gridwidth = 2;
		add(sbMinParticle,gbc);
		
		tfMinParticle = new TextField(String.valueOf(minParticle));
		tfMinParticle.addFocusListener(new FocusAdapter() {
			override def focusLost(e: FocusEvent): Unit = {
				try{minParticle = tfMinParticle.getText.toInt}
				catch{case ex: NumberFormatException => minParticle = minParticle}
			}
		});
		gbc.gridx = 4;	gbc.gridy = 2;	gbc.gridwidth = 1;
		add(tfMinParticle,gbc);

		label = new Label("Crop from border [nm]");
		gbc.gridx = 0;	gbc.gridy = 3;	gbc.gridwidth = 2;
		add(label,gbc);

		sbCrop = new Scrollbar(Scrollbar.HORIZONTAL, crop.toInt, 30, 0, MAX_CROP+30);
		sbCrop.addAdjustmentListener(new AdjustmentListener(){
			def adjustmentValueChanged(arg0: AdjustmentEvent) = {crop = arg0.getValue()}			
		});
		gbc.gridx = 2;	gbc.gridy = 3;	gbc.gridwidth = 2;
		add(sbCrop,gbc);

		tfCrop = new TextField(String.valueOf(crop));
		tfCrop.addFocusListener(new FocusAdapter() {
			override def focusLost(e: FocusEvent) {
				try{crop = tfCrop.getText.toInt}
				catch{case ex:NumberFormatException => crop = crop}
			}
		});
		gbc.gridx = 4;	gbc.gridy = 3;	gbc.gridwidth = 1;
		add(tfCrop,gbc);

		label = new Label("Actual [nm]/Pixels");
		gbc.gridx = 0;	gbc.gridy = 4;	gbc.gridwidth = 2;
		add(label,gbc);

		tfActual = new TextField("%.0f" format actualLength);
		tfActual.addFocusListener(new FocusAdapter() {			
			override def focusLost(arg0: FocusEvent) {
				try{actualLength = tfActual.getText.toDouble}
				catch{ case e:NumberFormatException => actualLength = actualLength }
			}
		});
		gbc.gridx = 2;	gbc.gridy = 4;	gbc.gridwidth = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		add(tfActual,gbc);

		tfPixels = new TextField("%.0f" format numPixelsToThat);
		tfPixels.addFocusListener(new FocusAdapter {			
			override def focusLost(arg0: FocusEvent) {
				try{numPixelsToThat = tfPixels.getText.toDouble
				}catch{case e: NumberFormatException => numPixelsToThat = numPixelsToThat}
			}
		});
		gbc.gridx = 3;	gbc.gridy = 4;	gbc.gridwidth = 1;
		add(tfPixels,gbc);
		
		chPreset = new Choice();
		for(a <- Array("Choose..","x20k","x40k","x80k","x200k"))
			chPreset.add(a)
		chPreset.addItemListener(new ItemListener() {
			def itemStateChanged(e: ItemEvent): Unit = {
				val name = chPreset.getSelectedItem();
				val str = Map("x20k"->"20k","x40k"->"40k","x80k"->"80k","x200k"->"200k")(name)
				if(str!=null)
					setParametersByMagnification(str)
			}
		});
		val filename: String = original.getTitle();
		System.out.println(filename);
		val mag = Array("20k","40k","80k","200k")
		//For some reason I can't shorten the following. That changes the behavior...
		val f = mag.filter({m:String => val ind = filename.indexOf(m); ind != -1})
		val item = f(0)
		if(item!=null){
			chPreset.select("x"+item)
			setParametersByMagnification(item)
		}else{
			chPreset.select("Choose..")		  
		}
		gbc.gridx = 4;	gbc.gridy = 4;	gbc.gridwidth = 1;
		add(chPreset,gbc);

		var b = new Button("Analyze");
		b.addActionListener(new ActionListener(){
		  def actionPerformed(e: ActionEvent) {analyze}
		});
		b.addKeyListener(IJ.getInstance());
		gbc.gridx = 0;	gbc.gridy = 5;	gbc.gridwidth = 2;
		add(b,gbc);

		b = new Button("Save result");
		b.addActionListener(new ActionListener(){
		  def actionPerformed(e: ActionEvent) {saveResult}
		});
		b.addKeyListener(IJ.getInstance());
		gbc.gridx = 2;	gbc.gridy = 5;	gbc.gridwidth = 2;
		add(b,gbc);

		//Stub: Add picture region
		
		taOutput = new TextArea();
		taOutput.setPreferredSize(new Dimension(500,100));
		gbc.gridx = 0;	gbc.gridy = 6;	gbc.gridwidth = 5;
		gbc.fill = GridBagConstraints.BOTH;
		add(taOutput,gbc);

		setSize(500,300);
		setResizable(false);
		pack();
	}
	def inPreset(actual: Double, pixels: Double): Int = {
		val E = 0.005;
		var ret: Int = 0
		val f = actual/pixels;
		if(abs(f-4.405)<E)	1
		else if(Math.abs(f-2.203)<E)	2
		else if(Math.abs(f-1.105)<E)	3
		else if(Math.abs(f-0.441)<E)	4
		else	0
	}
	
	def updateThreshold: Unit = {
	  if(windowActive){
		//Duplicate to work on the image
		if(newTask){
			output = original.duplicate
			output_p = output.getProcessor
			outputImgWidth = output_p.getRoi.width;
			outputImgHeight = output_p.getRoi.height;
			newTask = false
//			updateCrop
		}
		//Threshold
		output_p.setThreshold(minThreshold,maxThreshold,ImageProcessor.RED_LUT)
		output.show
		output.updateAndRepaintWindow
	  }
	}
	
	def updateCrop: Unit = {
		if(windowActive){
		val path = new GeneralPath();
		path.moveTo(crop/factor, crop/factor);
		path.lineTo(crop/factor, outputImgHeight-crop/factor);
		path.lineTo(outputImgWidth-crop/factor, outputImgHeight-crop/factor);
		path.lineTo(outputImgWidth-crop/factor, crop/factor);
		path.lineTo(crop/factor, crop/factor);
		output.setOverlay(path,Color.blue,new BasicStroke(1));
		}
	}

	def analyze: Unit = {
		if(original==null || output==null || output.getProcessor()==null) return;
		val imtype = original.getType
		if(!(imtype == ImagePlus.GRAY8 || imtype == ImagePlus.GRAY16 || imtype == ImagePlus.GRAY32)){
			IJ.error("Only grayscale images are supported.");
			return;
		}
		if(original.getStackSize()!=1){
			IJ.error("Stack Images are not supported.");
			return;
		}		

		//Particle detection
		println(opt,meas,null,minParticle,maxParticle)
		particleAnalyzer = new MyParticleAnalyzer(opt,meas,null,minParticle,maxParticle);
		particleAnalyzer.analyze(output);
		particles = particleAnalyzer.getPoints.toArray

		calculator = new SpacingCalculator(outputImgWidth,outputImgHeight,particles,factor);
		result = calculator.analyze(crop);

		//Draw lines between pairs
		output.setTitle("Hexagonal analysis: " + original.getTitle());
		output_p = output.getProcessor();
		output_p.setLineWidth(1);
		output_p.setColor(Color.GRAY);
		output_p.drawRect((crop/factor).toInt, (crop/factor).toInt, (outputImgWidth - crop/factor*2).toInt, (outputImgHeight-crop/factor*2).toInt);
		var p: PairElem = null
		while({p = result.getNextPair; p!=null}){
			output_p.drawLine((p.ax/factor).toInt, (p.ay/factor).toInt, (p.bx/factor).toInt, (p.by/factor).toInt);		
		}

		//Print summary
		taOutput.setText(result.getSummary);
		result.showResultsTable
		newTask = true;
	}

	def saveResult: Unit = {
		val originalFilename: String = original.getOriginalFileInfo().directory + 
		original.getOriginalFileInfo.fileName;
		var outFilename = originalFilename + ".txt";
		var num = 2
		while((new File(outFilename)).exists){
			outFilename = originalFilename + "."+ num.toString + ".txt"
			num += 1
		}
		try {
			val out = new PrintWriter(new BufferedWriter
					(new OutputStreamWriter(new FileOutputStream(outFilename), "UTF-8")));
			out.print("Hexagonal array analysis ver.1.2 (Nov 5, 2011; written in Scala), written by HK\n");
			out.print(("File: %s\nRoi: %s\nThreshold: %d %d\nMin particle: %d\nCrop from the border [nm]: %.2f\n"+
					"Magnification: %.1f nm/%d pixel (factor: %.3f)\n").format(
					originalFilename,roi.toString,
					maxThreshold,minThreshold, minParticle, crop, actualLength, numPixelsToThat.toInt, factor))
			out.println(result.getSummary)
			out.println(result.getAllDataString)
			out.close
		} catch{
		  case e:UnsupportedEncodingException =>
			e.printStackTrace();
		  case e: FileNotFoundException =>
			e.printStackTrace();
		}
	}

} // Particle_Test class


