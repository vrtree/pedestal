#version 330 core


const float MAX_TRACE_DISTANCE = .5;           // max trace distance
const float INTERSECTION_PRECISION = 0.0001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 200;
const float PI = 3.1415926535897932384626433832795;
const float PHI = 1.6180339887498948482045868343656;
uniform float uTime;
uniform vec3 uDimensions;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;
in vec3 vLight;

in vec2 vUV;

out vec4 color;

vec3 sPos[10];

float hash1( float n )
{
    return fract(sin(n)*43758.5453123);
}

float hash1( in vec2 f ) 
{ 
    return fract(sin(f.x+131.1*f.y)*43758.5453123); 
}

float hash( float n ) { return fract(sin(n)*753.5453123); }
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
  
    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}



float fNoise( vec3 p ){
   
    float n;
    
    p *= 200.;
    n += noise( p * 20. ) * .5;
    n += noise( p * 200. ) * .1;
    n += noise( p * 60. ) * .3;
    n += noise( p * 5. );
    
    return n;
   
    
}



float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r  );
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float opU( float d1, float d2 ){
  return min(d1,d2);
}

vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}



vec4 grow = vec4(1.0);

vec3 mapP( vec3 p )
{
    p.xyz += .300*sin(  7.0*p.yzx )*grow.x;
    p.xyz += 0.150*sin( 20.0*p.yzx )*grow.y;
    p.xyz += 0.075*sin( 30.5*p.yzx )*grow.z;
    return p;
}

vec2 map( vec3 q )
{

  vec2 res = vec2(-sdBox( q  , vec3( uDimensions.x * .5 + INTERSECTION_PRECISION * 5. ) ) , 1. );
  vec3 p = mapP( q );
  float d = length( p ) - (.4 * length( grow.xyz ) * uDimensions.x / 2.);

  d *= 0.05;

  res = opU( vec2( d , 2. ) , res);

  res = smoothU( res , vec2( sdSphere( q - vec3( .03 , 0.02 , .05 ) , .06) , 3.) , .005 );

 


  return res;
}




vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    float h =  INTERSECTION_PRECISION * 2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i < NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
     
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);
}

vec3 forwardSF( float i, float n) 
{
    float phi = 2.0*PI*fract(i/PHI);
    float zi = 1.0 - (2.0*i+1.0)/n;
    float sinTheta = sqrt( 1.0 - zi*zi);
    return vec3( cos(phi)*sinTheta, sin(phi)*sinTheta, zi);
}



float calcAO( in vec3 pos, in vec3 nor  )
{
  float ao = 0.0;
  for( int i=0; i<8; i++ )
  {
      vec3 ap = forwardSF( float(i) * 20., 128.0 );
      ap *= sign( dot(ap,nor) ) * hash1(float(i));
      ao += clamp( map( pos + nor*0.05 + ap*1.0 ).x*32.0, 0.0, 1.0 );
  }
  ao /= 8.0;
  
  return clamp( ao, 0.0, 1.0 );
}/*

float calcAO( in vec3 pos, in vec3 nor )
{
  float occ = 0.0;
  float sca = 2.;

  for( int i=0; i<20; i++ )
    {
        float hr = 0.01 + 0.612*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}*/


void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );

  grow.x = 1.3;
  grow.y = 1.2+ .2;
  grow.z = 1. + .2;

  //grow.x = .75 + .25 * sin( uTime * .4 + sin( uTime * .8 + sin( uTime * .2 )));
  //grow.y = .85 + .25 * sin( uTime * .38+ sin( uTime * .6 + sin( uTime * .5 )));
  //grow.z = .95 + .25 * sin( uTime * .36+ sin( uTime * .3 + sin( uTime * .9) ));


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


  vec3 col = vec3( 0. );
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);
    vec3 norm;


    vec3 lightDir = normalize(vLight - pos);

    
    norm = calcNormal( pos );

    if( abs(res.y - 2.) < .2 ){

      float occ = calcAO( pos , norm );

      col = vec3(occ* 10.) * (norm * .5 + .5);
      occ *= occ * 10.;

      col = occ * vec3( .5 , .3 , 1. )  + pow(max( 0., dot( reflect( lightDir , norm ), rd )), 5.) * vec3( 1. , .4 , 0. );
    }else if( abs(res.y - 3.) < .8  ){

      float x = fNoise( pos + vec3( .01 , 0. , 0.));
      float y = fNoise( pos + vec3( 0. , 0.01 , 0.));
      float z = fNoise( pos + vec3( 0. , 0. , 0.01));

      float dx = fNoise( pos - vec3( .01 , 0. , 0.));
      float dy = fNoise( pos - vec3( 0. , 0.01 , 0.));
      float dz = fNoise( pos - vec3( 0. , 0. , 0.01));
      //norm += .7 * normalize(vec3( x - dx , y - dy , z-dz));
      norm = normalize(norm);

      float spec = pow(max( 0., dot( reflect( lightDir , norm ), rd )), 5.);
      float face = max( 0., dot( -norm , rd ));
      float lamb = max( 0., dot( norm , lightDir ));
      spec = smoothstep( 0.1 , .5 , spec);
      //lamb = smoothstep( 0.4 , .5 , lamb);

      col = vec3( 1. , .5 , 0. ) * spec;
      col += vec3( 1. , .5 , 0. ) * lamb;
      col += vec3( 1. , .5 , 0. ) * (1.-face);
      //col = vec3( spec , 0., lamb);

    }else{

      //discard;
      //col = (norm * .5 + .5) * .3;
    }



  }


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    //col += vec3(.2);
  }


  color = vec4( col , 1. );



}