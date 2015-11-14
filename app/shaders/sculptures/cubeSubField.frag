#version 330 core

uniform vec3 uDimensions;
uniform float uTime;

const float MAX_TRACE_DISTANCE = 2.;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 20;
const float PI  = 3.14159;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;


float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    vec3 re = (q-p)/c;
    return sdSphere( q  , r * 1.9- .01 * length(re) );
}

vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

vec2 opS( vec2 d1, vec2 d2 )
{
    return  -d1.x > d2.x ? d2 : d1;
}


float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}


float subCube( vec3 pos ){

  float r = opRepSphere( pos , vec3( .05 * uDimensions.x * 2. )  , .025 * uDimensions.x * 2.8);
  r = opS( r ,sdBox( pos , vec3( .125 * uDimensions.x * 2. )) );

  return r;

}

mat4 rotateX(float angle){
    float c = cos(angle);
    float s = sin(angle);
  return mat4(1.0, 0.0, 0.0, 0.0, 0.0, c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0);
}


mat4 rotateZ(float angle){
    float c = cos(angle);
    float s = sin(angle);
  return mat4(c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);   
}
//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){

  vec2 res = vec2(-sdBox( pos  , vec3( uDimensions.x * .5 + INTERSECTION_PRECISION * 5. ) ) , 1. );


  mat4 rot = rotateX(.5 + uTime * .3 ) * rotateZ( .3 + uTime * .2 );

  vec3 nPos = (rot * vec4( pos , 1. )).xyz;

  res = opU( res , vec2( subCube( nPos ) , 1.));

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
}



void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


  vec3 col = vec3( 0. );
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);
    vec3 norm;

    
    norm = calcNormal( pos );
    float ao = calcAO( pos , norm );

    col = vec3( ao ) * (norm * .5 + .5);


    vec3 refrR = refract( rd , norm , 1. / 1.1 );
    vec3 refrG = refract( rd , norm , 1. / 1.2 );
    vec3 refrB = refract( rd , norm , 1. / 1.3 );


    float dR = -dot( refrR , norm );
    float dG = -dot( refrG , norm );
    float dB = -dot( refrB , norm );


    col = vec3( dR , dG , dB );




  }

  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3( .3 , .4 , .5);
  }





  color = vec4( col , 1. );



}