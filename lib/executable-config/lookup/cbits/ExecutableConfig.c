#include <assert.h>
#include <jni.h>
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>
#include <HaskellActivity.h>

static JNIEnv *getEnv() {

  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, &env, NULL);
  assert(attachResult == JNI_OK);

  return env;
}

jobject ExecutableConfig_getAssets() {
  JNIEnv *env = getEnv();

  jobject haskellActivity = HaskellActivity_get();
  jclass cls = (*env)->GetObjectClass(env, haskellActivity);
  jmethodID getAssets = (*env)->GetMethodID(env, cls, "getAssets", "()Landroid/content/res/AssetManager;");
  assert(getAssets);

  jobject assetManagerObj = (*env)->CallObjectMethod(env, haskellActivity, getAssets);
  assert(assetManagerObj);

  jobject assetManagerObjRef = (*env)->NewGlobalRef(env, assetManagerObj);

  (*env)->DeleteLocalRef(env, assetManagerObj);

  return assetManagerObjRef;
}

AAssetManager *ExecutableConfig_aassetManagerFromJava(jobject assetManagerObjRef) {
  JNIEnv *env = getEnv();
  return AAssetManager_fromJava(env, assetManagerObjRef);
}

void ExecutableConfig_freeAssetManager(jobject assetManagerObjRef) {
  JNIEnv *env = getEnv();
  (*env)->DeleteGlobalRef(env, assetManagerObjRef);
}
